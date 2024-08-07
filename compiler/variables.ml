(***********************************************************************)
(*                                                                     *)
(*                              CamlIDL                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Lesser General Public License LGPL v2.1 *)
(*                                                                     *)
(***********************************************************************)

(* $Id: variables.ml,v 1.12 2002-04-22 09:02:17 xleroy Exp $ *)

open Printf
open Utils
open Idltypes
open Cvttyp

(* Generate temporaries *)

let var_counter = ref 0
let temp_variables = ref([] : (string * idltype) list)

let new_var prefix =
  incr var_counter;
  prefix ^ string_of_int !var_counter
  
let new_c_variable ty =
  let name = new_var "_c" in
  temp_variables := (name, scrape_const ty) :: !temp_variables;
  name

let new_ml_variable () =
  let name = new_var "_v" in
  temp_variables := (name, Type_named("", "value")) :: !temp_variables;
  name

let new_ml_variable_block n =
  let name = new_var "_v" in
  let ty =
    Type_array({bound = Some(Expr_int(Int64.of_int n));
                size=None; length=None;
                is_string=false; is_bytes=false;
                maybe_null=false; null_terminated=false},
               Type_named("", "value")) in
  temp_variables := (name, ty) :: !temp_variables;
  name

let output_variable_declarations oc =
  List.iter
    (fun name_ty -> iprintf oc "%a;\n" out_c_decl name_ty)
    (List.rev !temp_variables);
  temp_variables := [];
  var_counter := 0

(* Zero an array of values *)

let init_value_block oc blk numvals =
  if numvals <= 4 then begin
    iprintf oc "";
    for i = 0 to numvals - 1 do
      fprintf oc "%s[%d] = " blk i
    done;
    fprintf oc "Val_unit;\n"
  end else begin
    let idx = new_var "_c" in
    iprintf oc "for (mlsize_t %s = 0; %s < %d; %s++) %s[%s] = Val_unit;\n"
               idx idx numvals idx blk idx
  end

(* Copy an array of values into the fields of a newly-allocated block *)

let copy_values_to_block oc src dst numvals =
  if numvals <= 4 then
    for i = 0 to numvals - 1 do
      iprintf oc "Field(%s, %d) = %s[%d];\n" dst i src i
    done
  else begin
    let idx = new_var "_c" in
    iprintf oc "for (mlsize_t %s = 0; %s < %d; %s++) Field(%s, %s) = %s[%s];\n"
               idx idx numvals idx dst idx src idx;
  end

(* Record if we need the context parameter *)

let need_context = ref false

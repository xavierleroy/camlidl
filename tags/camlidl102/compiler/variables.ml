(***********************************************************************)
(*                                                                     *)
(*                              CamlIDL                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0                *)
(*                                                                     *)
(***********************************************************************)

(* $Id: variables.ml,v 1.10 2000-08-19 11:04:58 xleroy Exp $ *)

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
  temp_variables := (name, ty) :: !temp_variables;
  name

let new_ml_variable () =
  let name = new_var "_v" in
  temp_variables := (name, Type_named("", "value")) :: !temp_variables;
  name

let new_ml_variable_block n =
  let name = new_var "_v" in
  let ty =
    Type_array({bound = Some(Expr_int n); size=None; length=None;
                is_string=false; maybe_null=false; null_terminated=false},
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
  if numvals <= 8 then begin
    iprintf oc "";
    for i = 0 to numvals - 1 do
      fprintf oc "%s[%d] = " blk i
    done;
    fprintf oc "0;\n"
  end else begin
    iprintf oc "memset(%s, 0, %d * sizeof(value));\n" blk numvals
  end

(* Copy an array of values into the fields of a newly-allocated block *)

let copy_values_to_block oc src dst numvals =
  if numvals <= 4 then
    for i = 0 to numvals - 1 do
      iprintf oc "Field(%s, %d) = %s[%d];\n" dst i src i
    done
  else begin
    let idx = new_var "_c" in
    iprintf oc "{ mlsize_t %s;\n" idx;
    increase_indent();
    iprintf oc "for (%s = 0; %s < %d; %s++) Field(%s, %s) = %s[%s];\n"
               idx idx numvals idx dst idx src idx;
    decrease_indent();
    iprintf oc "}\n"
  end

(* Record if we need the context parameter *)

let need_context = ref false

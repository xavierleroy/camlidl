(***********************************************************************)
(*                                                                     *)
(*                              CamlIDL                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(***********************************************************************)

(* $Id: struct.ml,v 1.10 2000-08-11 14:41:53 xleroy Exp $ *)

(* Handling of structures *)

open Printf
open Utils
open Variables
open Idltypes
open Cvttyp

(* Remove dependent fields (fields that are size_is, length_is,
   or switch_is of another field).  Also remove ignored pointers. *)

let is_dependent_field name fields =
  List.exists (fun f -> Lexpr.is_dependent name f.field_typ) fields

let is_ignored =
  function Type_pointer(Ignore, _) -> true | _ -> false

let remove_dependent_fields fields =
  list_filter
    (fun f ->
       not (is_dependent_field f.field_name fields || is_ignored f.field_typ))
    fields

(* Determine if all fields of a struct are floats *)

let rec is_float_field f =
  match f.field_typ with
    Type_float -> true
  | Type_double -> true
  | _ -> false (* FIXME: typedef double foo *)

let all_float_fields fl =
  List.for_all is_float_field fl

(* Translation from an ML record [v] to a C struct [c] *)
(* [sd] is the IDL declaration for the record type. *)

let struct_ml_to_c ml_to_c oc onstack sd v c =
  match remove_dependent_fields sd.sd_fields with
    [f] ->
      ml_to_c oc onstack (sprintf "%s." c) f.field_typ
                 v (sprintf "%s.%s" c f.field_name);
      List.iter
        (function {field_typ = Type_pointer(Ignore, _); field_name = n} ->
                    iprintf oc "%s.%s = NULL;\n" c n
                | _ -> ())
        sd.sd_fields
  | _ ->
      if all_float_fields sd.sd_fields then begin
        let rec convert_fields pos = function
          [] -> ()
        | f :: rem ->
            iprintf oc "%s.%s = Double_field(%s, %d);\n" c f.field_name v pos;
            convert_fields (pos + 1) rem in
        convert_fields 0 sd.sd_fields
      end else begin
        let rec convert_fields pos = function
          [] -> ()
        | {field_typ = Type_pointer(Ignore, _); field_name = n} :: rem ->
            iprintf oc "%s.%s = NULL;\n" c n;
            convert_fields pos rem
        | {field_name = n} :: rem when is_dependent_field n sd.sd_fields ->
            convert_fields pos rem
        | {field_typ = ty; field_name = n} :: rem ->
            let v' = new_ml_variable() in
            iprintf oc "%s = Field(%s, %d);\n" v' v pos;
            ml_to_c oc onstack (sprintf "%s." c) ty v' (sprintf "%s.%s" c n);
            convert_fields (pos + 1) rem in
        convert_fields 0 sd.sd_fields
      end

(* Translation from a C pointer struct [c] to an ML record [v].
   [sd] is the IDL declaration for the record type. *)

let struct_c_to_ml c_to_ml oc sd c v =
  match remove_dependent_fields sd.sd_fields with
    [f] ->
      c_to_ml oc (sprintf "%s." c) f.field_typ
                 (sprintf "%s.%s" c f.field_name) v
  | fields ->
      let nfields = List.length fields in
      if all_float_fields sd.sd_fields then begin
        iprintf oc
           "%s = camlidl_alloc_small(%d * Double_wosize, Double_tag);\n"
           v nfields;
        let rec convert_fields pos = function
          [] -> ()
        | f :: rem ->
            iprintf oc "Store_double_val(%s, %s.%s);\n" v c f.field_name;
            convert_fields (pos + 1) rem in
        convert_fields 0 sd.sd_fields
      end else begin
        let v' = new_ml_variable_block nfields in
        init_value_block oc v' nfields;
        iprintf oc "Begin_roots_block(%s, %d)\n" v' nfields;
        increase_indent();
        let rec convert_fields pos = function
          [] -> ()
        | {field_typ = Type_pointer(Ignore, _); field_name = n} :: rem ->
            convert_fields pos rem
        | {field_name = n} :: rem when is_dependent_field n sd.sd_fields ->
            convert_fields pos rem
        | {field_typ = ty; field_name = n} :: rem ->
            c_to_ml oc (sprintf "%s." c) ty
                       (sprintf "%s.%s" c n) (sprintf "%s[%d]" v' pos);
            convert_fields (pos + 1) rem in
        convert_fields 0 sd.sd_fields;
        iprintf oc "%s = camlidl_alloc_small(%d, 0);\n" v nfields;
        copy_values_to_block oc v' v nfields;
        decrease_indent();
        iprintf oc "End_roots()\n"
      end


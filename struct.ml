(* Handling of structures *)

open Printf
open Utils
open Variables
open Idltypes
open Cvttyp

(* Remove dependent fields (fields that are size_is, length_is,
   or switch_is of another field).  Also remove ignored pointers. *)

let is_dependent_field name fields =
  let is_param = function
      Var s -> s = name
    | Deref s -> s = name in
  let is_param_opt = function
      None -> false
    | Some re -> is_param re in
  List.exists
    (fun {field_name = name; field_typ = ty} ->
      match ty with
        Type_array(attr, ty) ->
          is_param_opt attr.size || is_param_opt attr.length
      | Type_union(name, discr) ->
          is_param discr
      | _ -> false)
    fields

let is_ignored =
  function Type_pointer(Ignore, _) -> true | _ -> false

let remove_dependent_fields fields =
  list_filter
    (fun f ->
       not (is_dependent_field f.field_name fields || is_ignored f.field_typ))
    fields

(* Translation from an ML record [v] to a C pointer struct [c] *)
(* [sd] is the IDL declaration for the record type. *)

let struct_ml_to_c ml_to_c oc sd v c =
  let rec convert_fields pos = function
    [] -> ()
  | {field_typ = Type_pointer(Ignore, _); field_name = n} :: rem ->
      iprintf oc "%s->%s = NULL;\n" c n;
      convert_fields pos rem
  | {field_name = n} :: rem when is_dependent_field n sd.sd_fields ->
      convert_fields pos rem
  | {field_typ = ty; field_name = n} :: rem ->
      let v' = new_ml_variable() in
      iprintf oc "%s = Field(%s, %d);\n" v' v pos;
      ml_to_c oc (sprintf "%s." c) ty v' (sprintf "%s.%s" c n);
      convert_fields (pos + 1) rem in
  convert_fields 0 sd.sd_fields

(* Translation from a C pointer struct [c] to an ML record [v].
   [sd] is the IDL declaration for the record type. *)

let struct_c_to_ml c_to_ml oc sd c v =
  let nfields = List.length(remove_dependent_fields sd.sd_fields) in
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
  iprintf oc "%s = alloc_small(%d, 0);\n" v nfields;
  copy_values_to_block oc v' v nfields;
  decrease_indent();
  iprintf oc "End_roots()\n"


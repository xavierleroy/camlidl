(* Handling of structures *)

open Printf
open Utils
open Variables
open Idltypes
open Cvttyp
open Cvtval

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
  function Type_pointer({ignore=true}, _) -> true | _ -> false

let remove_dependent_fields fields =
  let rec remove = function
    [] -> []
  | field :: rem ->
      if is_dependent_field field.field_name fields
      || is_ignored field.field_typ
      then remove rem
      else field :: remove rem
  in remove fields

(* Convert an IDL struct declaration to an ML record declaration *)

let declare_ml_record oc sd =
  fprintf oc "struct_%s = {\n" sd.sd_name;
  List.iter
    (fun f ->
      fprintf oc "  %s: %a; \n" f.field_name out_ml_type f.field_typ)
    (remove_dependent_fields sd.sd_fields);
  fprintf oc "}\n"

(* Forward declaration of the translation functions *)

let declare_struct_transl oc sd =
  fprintf oc "void _camlidl_ml2c_%s_struct_%s(value, struct %s *);\n"
             !module_name sd.sd_name sd.sd_name;
  fprintf oc "value _camlidl_c2ml_%s_struct_%s(struct %s *);\n\n"
             !module_name sd.sd_name sd.sd_name

(* Translation function from an ML record to a C struct *)

let struct_ml_to_c oc sd =
  current_function := sprintf "struct %s" sd.sd_name;
  let v = new_var "_v" in
  let c = new_var "_c" in
  fprintf oc "void _camlidl_ml2c_%s_struct_%s(value %s, struct %s * %s)\n"
             !module_name sd.sd_name v sd.sd_name c;
  fprintf oc "{\n";
  let pc = divert_output() in
  let rec convert_fields pos = function
    [] -> ()
  | {field_typ = Type_pointer({ignore=true}, _); field_name = n} :: rem ->
      iprintf pc "%s->%s = NULL;\n" c n;
      convert_fields pos rem
  | {field_name = n} :: rem when is_dependent_field n sd.sd_fields ->
      convert_fields pos rem
  | {field_typ = ty; field_name = n} :: rem ->
      let v' = new_ml_variable() in
      iprintf pc "%s = Field(%s, %d);\n" v' v pos;
      ml_to_c pc (sprintf "%s->" c) ty v' (sprintf "%s->%s" c n);
      convert_fields (pos + 1) rem in
  convert_fields 0 sd.sd_fields;
  output_variable_declarations oc;
  end_diversion oc;
  fprintf oc "}\n\n";
  current_function := ""

(* Translation function from a C struct to an ML record *)

let struct_c_to_ml oc sd =
  current_function := sprintf "struct %s" sd.sd_name;
  let c = new_var "_c" in
  let idx = new_c_variable (Type_named "mlsize_t") in
  fprintf oc "value _camlidl_c2ml_%s_struct_%s(struct %s * %s)\n"
             !module_name sd.sd_name sd.sd_name c;
  fprintf oc "{\n";
  let pc = divert_output() in
  let rec convert_fields pos = function
    [] -> pos
  | {field_typ = Type_pointer({ignore=true}, _); field_name = n} :: rem ->
      convert_fields pos rem
  | {field_name = n} :: rem when is_dependent_field n sd.sd_fields ->
      convert_fields pos rem
  | {field_typ = ty; field_name = n} :: rem ->
      c_to_ml pc (sprintf "%s->" c) ty
                 (sprintf "%s->%s" c n) (sprintf "_vres[%d]" pos);
      convert_fields (pos + 1) rem in
  let nfields = convert_fields 0 sd.sd_fields in
  output_variable_declarations oc;
  fprintf oc "  value _vresult;\n";
  fprintf oc "  value _vres[%d] = {0, };\n\n" nfields;
  fprintf oc "  Begin_roots_block(_vres, %d)\n" nfields;
  end_diversion oc;
  fprintf oc "  _vresult = alloc_small(%d, 0);\n" nfields;
  copy_values_to_block oc "_vres" "_vresult" nfields;
  fprintf oc "  return _vresult;\n";
  fprintf oc "}\n\n";
  current_function := ""

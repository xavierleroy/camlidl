open Printf
open Idltypes

(* Convert an IDL type to a C declarator *)

let integer_type = function
    Int -> "int"
  | Long -> "long"
  | Small -> "signed char"
  | Short -> "short"
  | Char -> "char"
  | UInt -> "unsigned int"
  | ULong -> "unsigned long"
  | USmall -> "unsigned char"
  | UShort -> "unsigned short"
  | UChar -> "unsigned char"
  | Byte -> "unsigned char"
  | Boolean -> "int"

let rec out_c_decl oc (id, ty) =
  match ty with
    Type_int kind -> fprintf oc "%s %s" (integer_type kind) id
  | Type_float -> fprintf oc "float %s" id
  | Type_double -> fprintf oc "double %s" id
  | Type_void -> fprintf oc "void %s" id
  | Type_struct sd ->
      assert (sd.sd_name <> "");
      fprintf oc "struct %s %s" sd.sd_name id
  | Type_union(ud, discr) ->
      assert (ud.ud_name <> "");
      fprintf oc "union %s %s" ud.ud_name id
  | Type_enum (en, attr) ->
      fprintf oc "int %s" id
      (* Alternatively, one could do:
           assert (en.en_name <> "");
           fprintf oc "enum %s %s" en.en_name id
         but this forces the enum to be declared beforehand on the C side *)
  | Type_named s ->
      fprintf oc "%s %s" s id
  | Type_pointer(attr, (Type_array(_, _) as ty)) ->
      out_c_decl oc (sprintf "(*%s)" id, ty)
  | Type_pointer(attr, ty) ->
      out_c_decl oc (sprintf "*%s" id, ty)
  | Type_array(attr, ty) ->
      let id' =
        match attr.bound with
          Some n -> sprintf "%s[%d]" id n
        | None -> sprintf "*%s" id in
      out_c_decl oc (id', ty)
  | Type_interface intf_name ->
      fprintf oc "interface %s %s" intf_name id

(* Convert an IDL type to a C type *)

let out_c_type oc ty = out_c_decl oc ("", ty)

(* Convert an IDL type to an ML type *)

let rec out_ml_type oc ty =
  match ty with
    Type_int Boolean -> output_string oc "bool"
  | Type_int (Char | UChar) -> output_string oc "char"
  | Type_int kind -> output_string oc "int"
  | Type_float | Type_double -> output_string oc "float"
  | Type_void -> output_string oc "void"
  | Type_named s -> output_string oc s
  | Type_interface s -> output_string oc s
  | Type_struct sd ->
      if sd.sd_name = ""
      then fprintf oc "struct_%d" sd.sd_stamp
      else fprintf oc "%s" sd.sd_name
  | Type_union(ud, discr) ->
      if ud.ud_name = ""
      then fprintf oc "union_%d" ud.ud_stamp
      else fprintf oc "%s" ud.ud_name
  | Type_enum (en, attr) ->
      if en.en_name = ""
      then fprintf oc "enum_%d" en.en_stamp
      else fprintf oc "%s" en.en_name;
      if attr.bitset then fprintf oc " list"
  | Type_pointer(kind, ty) ->
      begin match kind with
        Ref -> out_ml_type oc ty
      | Unique -> fprintf oc "%a option" out_ml_type ty
      | Ptr -> fprintf oc "%a opaque" out_ml_type ty
      | Ignore -> assert false
      end
  | Type_array(attr, ty) ->
      if attr.is_string
      then fprintf oc "string"
      else fprintf oc "%a array" out_ml_type ty

(* Output a list of ML types *)

let out_ml_types oc sep types =
  match types with
    [] -> fprintf oc "unit"
  | (_, ty1) :: tyl ->
      out_ml_type oc ty1;
      List.iter (fun (_, ty) -> fprintf oc " %s " sep; out_ml_type oc ty) tyl

(* Output a reference to a restricted expression *)

let string_of_restr_expr = function Var s -> s | Deref s -> "*" ^ s

let out_restr_expr oc e = output_string oc (string_of_restr_expr e)


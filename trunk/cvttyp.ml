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
  | Type_struct s -> fprintf oc "struct %s %s" s id
  | Type_union(s, discr) -> fprintf oc "union %s %s" s id
  | Type_enum s -> fprintf oc "enum %s %s" s id
  | Type_named s -> fprintf oc "%s %s" s id
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
  | Type_struct s -> fprintf oc "struct_%s" s
  | Type_union(s, discr) -> fprintf oc "union_%s" s
  | Type_enum s -> fprintf oc "enum_%s" s
  | Type_named s -> output_string oc s
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


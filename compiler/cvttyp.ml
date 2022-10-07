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

(* $Id: cvttyp.ml,v 1.27 2004-07-08 11:24:43 xleroy Exp $ *)

open Utils
open Printf
open Idltypes

(* Convert an IDL type to a C declarator *)

let integer_type = function
    Int -> "int"
  | Long -> "long"
  | Hyper -> Config.int64_type
  | Small -> "signed char"
  | Short -> "short"
  | Char -> "char"
  | UInt -> "unsigned int"
  | ULong -> "unsigned long"
  | UHyper -> Config.uint64_type
  | USmall -> "unsigned char"
  | UShort -> "unsigned short"
  | UChar -> "unsigned char"
  | SChar -> "signed char"
  | Byte -> "unsigned char"
  | Boolean -> "int"

let parenthesize_if_pointer id =
  if String.length id > 0 && id.[0] = '*' then "(" ^ id ^ ")" else id

let rec out_c_decl oc (id, ty) =
  match ty with
    Type_int(kind, repr) -> fprintf oc "%s %s" (integer_type kind) id
  | Type_float -> fprintf oc "float %s" id
  | Type_double -> fprintf oc "double %s" id
  | Type_void -> fprintf oc "void %s" id
  | Type_struct sd ->
      if sd.sd_name <> ""
      then fprintf oc "struct %s %s" sd.sd_name id
      else fprintf oc "%a %s" out_struct sd id
  | Type_union(ud, discr) ->
      if ud.ud_name <> ""
      then fprintf oc "union %s %s" ud.ud_name id
      else fprintf oc "%a %s" out_union ud id
  | Type_enum (en, attr) ->
      if en.en_name <> ""
      then fprintf oc "int %s" id
      else fprintf oc "%a %s" out_enum en id
  | Type_named(modl, ty_name) ->
      fprintf oc "%s %s" ty_name id
  | Type_pointer(attr, ty) ->
      out_c_decl oc (sprintf "*%s" id, ty)
  | Type_array(attr, ty) ->
      let id' =
        match attr.bound with
          Some n ->
            sprintf "%s[%d]" (parenthesize_if_pointer id) (Lexpr.eval_int n)
        | None ->
            sprintf "*%s" id in
      out_c_decl oc (id', ty)
  | Type_bigarray(attr, ty) ->
      out_c_decl oc (sprintf "*%s" id, ty)
  | Type_interface(modl, intf_name) ->
      fprintf oc "struct %s %s" intf_name id
  | Type_const ty' ->
      out_c_decl oc (sprintf "const %s" id, ty')

and out_struct oc sd =
  fprintf oc "struct ";
  if sd.sd_name <> "" then fprintf oc "%s " sd.sd_name;
  fprintf oc "{\n";
  increase_indent();
  List.iter (out_field oc) sd.sd_fields;
  decrease_indent();
  iprintf oc "}"

and out_field oc f =
  iprintf oc "%a;\n" out_c_decl (f.field_name, f.field_typ)

and out_union oc ud =
  fprintf oc "union ";
  if ud.ud_name <> "" then fprintf oc "%s " ud.ud_name;
  fprintf oc "{\n";
  increase_indent();
  List.iter (out_case oc) ud.ud_cases;
  decrease_indent();
  iprintf oc "}"

and out_case oc c =
  match c.case_field with None -> () | Some f -> out_field oc f

and out_enum oc en =
  fprintf oc "enum ";
  if en.en_name <> "" then fprintf oc "%s " en.en_name;
  fprintf oc "{\n";
  increase_indent();
  List.iter (out_enum_const oc) en.en_consts;
  decrease_indent();
  iprintf oc "}"

and out_enum_const oc cst =
  fprintf oc "%s" cst.const_name;
  begin match cst.const_val with
    None -> ()
  | Some le -> fprintf oc " = %a" Lexpr.output (Prefix.empty, le)
  end;
  fprintf oc ",\n"

(* Convert an IDL type to a C type *)

let out_c_type oc ty = out_c_decl oc ("", ty)

(* Print an ML type name, qualified if necessary *)

let out_mltype_name oc (modl, name) =
  if modl <> !module_name then fprintf oc "%s." (String.capitalize_ascii modl);
  output_string oc (String.uncapitalize_ascii name)

(* Same, but use stamp if no name is provided *)

let out_mltype_stamp oc kind modl name stamp =
  if modl <> !module_name then fprintf oc "%s." (String.capitalize_ascii modl);
  if name = ""
  then fprintf oc "%s_%d" kind stamp
  else output_string oc (String.uncapitalize_ascii name)

(* Convert an IDL type to an ML bigarray element type *)

let rec ml_bigarray_kind ty =
  match ty with
    Type_int((Char | UChar | Byte), _) -> "Bigarray.int8_unsigned_elt"
  | Type_int((SChar | Small), _) -> "Bigarray.int8_signed_elt"
  | Type_int(Short, _) -> "Bigarray.int16_signed_elt"
  | Type_int(UShort, _) -> "Bigarray.int16_unsigned_elt"
  | Type_int((Int | UInt), _) -> "Bigarray.int32_elt"
  | Type_int((Long | ULong), I64) -> "Bigarray.int64_elt"
  | Type_int((Long | ULong), _) -> "Bigarray.nativeint_elt"
  | Type_int((Hyper | UHyper), _) -> "Bigarray.int64_elt"
  | Type_float -> "Bigarray.float32_elt"
  | Type_double -> "Bigarray.float64_elt"
  | Type_const ty -> ml_bigarray_kind ty
  | _ -> assert false

(* Convert an IDL type to an ML type *)

let rec out_ml_type oc ty =
  match ty with
    Type_int(Boolean, _) -> output_string oc "bool"
  | Type_int((Char | UChar | SChar), _) -> output_string oc "char"
  | Type_int(_, Iunboxed) -> output_string oc "int"
  | Type_int(_, Inative) -> output_string oc "nativeint"
  | Type_int(_, I32) -> output_string oc "int32"
  | Type_int(_, I64) -> output_string oc "int64"
  | Type_float | Type_double -> output_string oc "float"
  | Type_void -> output_string oc "void"
  | Type_named(modl, name) -> out_mltype_name oc (modl, name)
  | Type_struct sd ->
      out_mltype_stamp oc "struct" sd.sd_mod sd.sd_name sd.sd_stamp
  | Type_union(ud, discr) ->
      out_mltype_stamp oc "union" ud.ud_mod ud.ud_name ud.ud_stamp
  | Type_enum (en, attr) ->
      out_mltype_stamp oc "enum" en.en_mod en.en_name en.en_stamp;
      if attr.bitset then fprintf oc " list"
  | Type_pointer(kind, ty) ->
      begin match kind with
        Ref -> out_ml_type oc ty
      | Unique -> fprintf oc "%a option" out_ml_type ty
      | Ptr -> fprintf oc "%a Com.opaque" out_ml_type ty
      | Ignore -> assert false
      end
  | Type_array(attr, ty) ->
      if attr.is_string then fprintf oc "string"
      else if attr.is_bytes then fprintf oc "bytes"
      else fprintf oc "%a array" out_ml_type ty;
      if attr.maybe_null
      then fprintf oc " option"
  | Type_bigarray(attr, ty) ->
      let layout =
        if attr.fortran_layout
        then "Bigarray.fortran_layout"
        else "Bigarray.c_layout" in
      let typeconstr =
        match List.length attr.dims with
          1 -> "Bigarray.Array1.t"
        | 2 -> "Bigarray.Array2.t"
        | 3 -> "Bigarray.Array3.t"
        | _ -> "Bigarray.Genarray.t" in
      fprintf oc "(%a, %s, %s) %s"
        out_ml_type ty (ml_bigarray_kind ty) layout typeconstr;
      if attr.bigarray_maybe_null
      then fprintf oc " option"
  | Type_interface(modl, name) ->
      fprintf oc "%a Com.interface" out_mltype_name (modl, name)
  | Type_const ty' ->
      out_ml_type oc ty'

(* Output a list of ML types *)

let out_ml_types oc sep types =
  match types with
    [] -> fprintf oc "unit"
  | (_, ty1) :: tyl ->
      out_ml_type oc ty1;
      List.iter (fun (_, ty) -> fprintf oc " %s " sep; out_ml_type oc ty) tyl

(* Expand typedef and const in type *)
let rec scrape_type = function
    Type_named(modname, tyname) -> scrape_type (!Lexpr.expand_typedef tyname)
  | Type_const ty -> scrape_type ty
  | ty -> ty

(* Remove leading "const" from a type *)
let rec scrape_const = function
    Type_const ty -> scrape_const ty
  | ty -> ty

(* Determine if a type is an ignored pointer *)
let rec is_ignored = function
    Type_pointer(Ignore, _) -> true
  | Type_const ty -> is_ignored ty
  | _ -> false

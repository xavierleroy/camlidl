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

(* $Id: constdecl.ml,v 1.14 2002-01-16 16:15:30 xleroy Exp $ *)

(* Handling of constant declarations *)

open Printf
open Utils
open Idltypes
open Lexpr
open Cvttyp

type constant_decl =
  { cd_name: string; cd_type: idltype; cd_value: lexpr }

(* Record the value of a constant declaration *)

let record c =
  Lexpr.bind_const c.cd_name (cast_value c.cd_type (eval c.cd_value))

(* Declare the constant in ML *)

let ml_declaration oc c =
  fprintf oc "val %s : " (String.uncapitalize_ascii c.cd_name);
  match scrape_type c.cd_type with
    Type_int(_, _) ->
      fprintf oc "%a\n" out_ml_type c.cd_type
  | Type_pointer(_, Type_int((Char | UChar | SChar), _)) |
    Type_array({is_string = true}, _) ->
      fprintf oc "string\n"
  | _ ->
      error "unsupported type for constant expression"

(* #define the constant in C *)

let c_declaration oc c =
  fprintf oc "#define %s (%a)\n\n"
             c.cd_name Lexpr.output (Prefix.empty, c.cd_value)

(* Generate the ML let binding corresponding to the constant declaration *)

let ml_definition oc c =
  let v = eval c.cd_value in
  let name = String.uncapitalize_ascii c.cd_name in
  match scrape_type c.cd_type with
    Type_int((Char | UChar | SChar), _) ->
      fprintf oc "let %s = '%s'\n\n"
                 name (Char.escaped (Char.chr ((int_val v) land 0xFF)))
  | Type_int(Boolean, _) ->
      fprintf oc "let %s = %s\n\n"
                 name (if is_true v then "true" else "false")
  | Type_int(_, Iunboxed) ->
      fprintf oc "let %s = %d\n\n"
                 name (int_val v)
  | Type_int(_, Inative) ->
      fprintf oc "let %s = Nativeint.of_string \"%nd\"\n\n"
                 name (nativeint_val v)
  | Type_int(_, I32) ->
      fprintf oc "let %s = Int32.of_string \"%ld\"\n\n"
                 name (int32_val v)
  | Type_int(_, I64) ->
      fprintf oc "let %s = Int64.of_string \"%Ld\"\n\n"
                 name (int64_val v)
  | Type_pointer(_, Type_int((Char | UChar | SChar), _)) |
    Type_array({is_string = true}, _) ->
      fprintf oc "let %s = \"%s\"\n\n"
                 name (String.escaped (string_val v))
  | _ ->
      error "unsupported type for constant expression"

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

(* $Id: constdecl.ml,v 1.12 2001-06-29 13:29:59 xleroy Exp $ *)

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
  Lexpr.bind_const c.cd_name (eval c.cd_value)

(* Declare the constant in ML *)

let ml_declaration oc c =
  fprintf oc "val %s : %a\n"
             (String.uncapitalize c.cd_name) out_ml_type c.cd_type

(* #define the constant in C *)

let c_declaration oc c =
  fprintf oc "#define %s (%a)\n\n" c.cd_name Lexpr.output ("", c.cd_value)

(* Generate the ML let binding corresponding to the constant declaration *)

let ml_definition oc c =
  let v = eval c.cd_value in
  let name = String.uncapitalize c.cd_name in
  match (scrape_type c.cd_type, v) with
    (Type_int((Char | UChar | SChar), _), Cst_int n) ->
      fprintf oc "let %s = '%s'\n\n"
                 name (Char.escaped (Char.chr (n land 0xFF)))
  | (Type_int(Boolean, _), Cst_int n) ->
      fprintf oc "let %s = %s\n\n"
                 name (if n <> 0 then "true" else "false")
  | (Type_int(_, Iunboxed), Cst_int n) ->
      fprintf oc "let %s = %d\n\n"
                 name n
  | (Type_int(_, Inative), Cst_int n) ->
      fprintf oc "let %s = Nativeint.of_int %d\n\n"
                 name n
  | (Type_int(_, I32), Cst_int n) ->
      fprintf oc "let %s = Int32.of_int %d\n\n"
                 name n
  | (Type_int(_, I64), Cst_int n) ->
      fprintf oc "let %s = Int64.of_int %d\n\n"
                 name n
  | (Type_pointer(_, Type_int((Char | UChar | SChar), _)), Cst_string s) ->
      fprintf oc "let %s = \"%s\"\n\n"
                 name (String.escaped s)
  | _ ->
      error (sprintf "type mismatch in constant %s" c.cd_name)

(* Handling of constant declarations *)

open Printf
open Utils
open Idltypes
open Lexpr
open Cvttyp

type constant_decl =
  { cd_name: string; cd_type: idltype; cd_value: lexpr }

(* Generate the ML let binding corresponding to the constant declaration *)

let ml_declaration oc c =
  fprintf oc "val %s : %a\n"
             (String.uncapitalize c.cd_name) out_ml_type c.cd_type
  
let ml_definition oc c =
  let v = eval c.cd_value in
  Lexpr.bind_const c.cd_name v;
  let name = String.uncapitalize c.cd_name in
  match (c.cd_type, v) with
    (Type_int(Char | UChar | SChar), Cst_int n) ->
      fprintf oc "let %s = '%s'\n\n"
                 name (Char.escaped (Char.chr (n land 0xFF)))
  | (Type_int(Boolean), Cst_int n) ->
      fprintf oc "let %s = %s\n\n"
                 name (if n <> 0 then "true" else "false")
  | (Type_int(_), Cst_int n) ->
      fprintf oc "let %s = %d\n\n"
                 name n
  | (Type_pointer(_, Type_int(Char | UChar | SChar)), Cst_string s) ->
      fprintf oc "let %s = \"%s\"\n\n"
                 name (String.escaped s)
  | _ ->
      error (sprintf "type mismatch in constant %s" c.cd_name)

(* Import a constant declaration *)

let import c =
  Lexpr.bind_const c.cd_name (eval c.cd_value)

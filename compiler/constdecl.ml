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
  fprintf oc "val %s : %a\n" c.cd_name out_ml_type c.cd_type
  
let ml_definition oc c =
  match (c.cd_type, eval c.cd_value) with
    (Type_int(Char | UChar | SChar), Cst_int n) ->
      fprintf oc "let %s = '%s'\n\n"
                 c.cd_name (Char.escaped (Char.chr (n land 0xFF)))
  | (Type_int(Boolean), Cst_int n) ->
      fprintf oc "let %s = %s\n\n"
                 c.cd_name (if n <> 0 then "true" else "false")
  | (Type_int(_), Cst_int n) ->
      fprintf oc "let %s = %d\n\n"
                 c.cd_name n
  | (Type_pointer(_, Type_int(Char | UChar | SChar)), Cst_string s) ->
      fprintf oc "let %s = \"%s\"\n\n"
                 c.cd_name (String.escaped s)
  | _ ->
      error (sprintf "type mismatch in constant %s" c.cd_name)

(* Handling of constant declarations *)

open Printf
open Utils
open Idltypes

(* Generate the ML let binding corresponding to the constant declaration *)

let ml_declaration oc c =
  match c with
    {cd_type = Type_int(Char | UChar); cd_value = Cst_int n} ->
      fprintf oc "let %s = '%s'\n\n"
                 c.cd_name (Char.escaped (Char.chr (n land 0xFF)))
  | {cd_type = Type_int(Boolean); cd_value = Cst_int n} ->
      fprintf oc "let %s = %s\n\n"
                 c.cd_name (if n <> 0 then "true" else "false")
  | {cd_type = Type_int(_); cd_value = Cst_int n} ->
      fprintf oc "let %s = %d\n\n"
                 c.cd_name n
  | {cd_type = Type_pointer(_, Type_int(Char | UChar));
     cd_value = Cst_string s} ->
      fprintf oc "let %s = \"%s\"\n\n"
                 c.cd_name (String.escaped s)
  | _ ->
      error (sprintf "type mismatch in constant %s" c.cd_name)

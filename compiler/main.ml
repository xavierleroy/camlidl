open Printf
open Utils
open Idltypes
open Intfgen
open Stubgen

let process_file name =
  let pref =
    if Filename.check_suffix name ".idl"
    then Filename.chop_suffix name ".idl"
    else name in
  module_name := Filename.basename pref;
  let ic = open_in name in
  let lb = Lexing.from_channel ic in
  let intf =
    try
      Parser_simple.file Lexer_simple.token lb
    with Parsing.Parse_error ->
      eprintf "File %s, character %d: syntax error\n"
              name (Lexing.lexeme_start lb);
      exit 2 in
  close_in ic;
  let (nintf, all_type_decls) = Normalize.interface intf in
  let oc = open_out (pref ^ ".mli") in
  begin try
    gen_ml_decls oc nintf all_type_decls;
    close_out oc
  with x ->
    close_out oc; Sys.remove (pref ^ ".mli"); raise x
  end;
  let oc = open_out (pref ^ ".c") in
  begin try
    gen_c_stub oc nintf;
    close_out oc
  with x ->
    close_out oc; Sys.remove (pref ^ ".c"); raise x
  end

let _ =
  try
    Arg.parse [] process_file
      "Usage: camlidl <.idl file> ... <.idl file>\n"
  with Error ->
    exit 2

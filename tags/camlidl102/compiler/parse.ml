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

(* $Id: parse.ml,v 1.6 2001-07-30 14:23:31 xleroy Exp $ *)

(* Source parsing *)

open Printf
open Utils
open Linenum

let read_source_file sourcename filename =
  let ic = open_in_bin filename in
  let lb = Lexing.from_channel ic in
  Linenum.current_file := filename;
  Linenum.current_lexbuf := lb;
  try
    let res = Parser_midl.file Lexer_midl.token lb in
    close_in ic;
    res
  with Parsing.Parse_error ->
         close_in ic;
         eprintf "%t: syntax error\n" print_location;
         raise Error
     | Lexer_midl.Lex_error msg ->
         close_in ic;
         eprintf "%t: %s\n" print_location msg;
         raise Error

let read_file filename =
  if not !Clflags.use_cpp then
    read_source_file filename filename
  else begin
    let tempfile = Filename.temp_file "camlidl" ".idl" in
    try
      if Sys.command
           (sprintf "%s %s %s %s > %s"
                    !Clflags.preprocessor
                    (String.concat " "
                        (List.map (fun s -> "-I" ^ s) !Clflags.search_path))
                    (String.concat " "
                        (List.map (fun s -> "-D" ^ s) !Clflags.prepro_defines))
                    filename
                    tempfile)
         <> 0
       then error "error during preprocessing";
       let r = read_source_file filename tempfile in
       remove_file tempfile;
       r
     with x ->
       remove_file tempfile;
       raise x
  end

let _ =
  Parse_aux.read_file := read_file

(* Source parsing *)

open Printf
open Utils

let report_syntax_error filename pos msg =
  let (sourcename, lineno, startline) = Linenum.for_position filename pos in
  eprintf "File %s, line %d, character %d: %s\n"
          sourcename lineno (pos - startline) msg

let read_source_file sourcename filename =
  let ic = open_in filename in
  let lb = Lexing.from_channel ic in
  try
    let res = Parser_midl.file Lexer_midl.token lb in
    close_in ic;
    res
  with Parsing.Parse_error ->
         close_in ic;
         report_syntax_error filename (Lexing.lexeme_start lb) "syntax error";
         raise Error
     | Lexer_midl.Lex_error msg ->
         close_in ic;
         report_syntax_error filename (Lexing.lexeme_start lb) msg;
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

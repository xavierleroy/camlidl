open Printf
open Clflags
open Utils
open Idltypes
open File

let process_file name =
  let pref =
    if Filename.check_suffix name ".idl"
    then Filename.chop_suffix name ".idl"
    else name in
  let (intf, import_decls) = Normalize.process_file name in
  let oc = open_out (pref ^ ".mli") in
  begin try
    gen_mli_file oc intf;
    close_out oc
  with x ->
    close_out oc; Sys.remove (pref ^ ".ml"); raise x
  end;
  let oc = open_out (pref ^ ".ml") in
  begin try
    gen_ml_file oc intf;
    close_out oc
  with x ->
    close_out oc; Sys.remove (pref ^ ".ml"); raise x
  end;
  let oc = open_out (pref ^ ".c") in
  begin try
    gen_c_stub oc import_decls intf;
    close_out oc
  with x ->
    close_out oc; Sys.remove (pref ^ ".c"); raise x
  end

let _ =
  try
    Arg.parse
      ["-I", Arg.String(fun s -> search_path := !search_path @ [s]),
         "<dir>  Add directory to search path";
       "-header", Arg.Set include_header,
         "  Generate a #include \"filename.h\" in the C stub code (default)";
       "-noheader", Arg.Clear include_header,
         "  Do not generate any extra #include in the C stub code";
       "-D", Arg.String(fun s -> prepro_defines := !prepro_defines @ [s]),
         "<symbol>  Pass -D<symbol> to the C preprocessor";
       "-cpp", Arg.Set use_cpp,
         "  Pass the .idl files through the C preprocessor (default)";
       "-nocpp", Arg.Clear use_cpp,
         "  Do not pass the .idl files through the C preprocessor";
       "-prepro", Arg.String(fun s -> preprocessor := s),
         "<cmd>  Use <cmd> as the preprocessor instead of the C preprocessor"
      ]
      process_file
      "Usage: camlidl [options]<.idl file> ... <.idl file>\nOptions are:\n"
  with Error ->
    exit 2

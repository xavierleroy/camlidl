open Printf
open Utils
open Idltypes
open File

let process_file name =
  let pref =
    if Filename.check_suffix name ".idl"
    then Filename.chop_suffix name ".idl"
    else name in
  let (intf, import_decls, all_type_decls) = Normalize.process_file name in
  let oc = open_out (pref ^ ".mli") in
  begin try
    gen_mli_file oc intf all_type_decls;
    close_out oc
  with x ->
    close_out oc; Sys.remove (pref ^ ".ml"); raise x
  end;
  let oc = open_out (pref ^ ".ml") in
  begin try
    gen_ml_file oc intf all_type_decls;
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
         "<dir>  Adds directory to search path"]
      process_file
      "Usage: camlidl [options]<.idl file> ... <.idl file>\nOptions are:\n"
  with Error ->
    exit 2

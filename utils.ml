(* Utility functions *)

open Printf

(* Indented printf *)

let current_indentation = ref 2

let iprintf oc fmt =
  for i = 1 to !current_indentation do output_char oc ' ' done;
  fprintf oc fmt

let increase_indent() =
  current_indentation := !current_indentation + 2

let decrease_indent() =
  current_indentation := !current_indentation - 2

(* Divert output to a temp file *)

let temp_file = ref ""
let temp_out = ref stdout

let divert_output() =
  let f = Filename.temp_file "camlidl" ".c" in
  let oc = open_out f in
  temp_file := f; temp_out := oc; oc

let end_diversion oc =
  close_out !temp_out;
  let ic = open_in !temp_file in
  let buffer = String.create 256 in
  let rec copy() =
    let n = input ic buffer 0 256 in
    if n > 0 then (output oc buffer 0 n; copy()) in
  copy();
  close_in ic;
  Sys.remove !temp_file

(* Remember current module name and current function name *)

let module_name = ref "Mod"
let current_function = ref ""

(* Emit error messages *)

exception Error

let error msg =
  eprintf "%s.idl" !module_name;
  if !current_function <> "" then eprintf ", function %s" !current_function;
  eprintf ": %s\n" msg;
  raise Error



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

(* $Id: utils.ml,v 1.10 2000-08-19 11:04:58 xleroy Exp $ *)

(* Utility functions *)

open Printf

(* Indented printf *)

let current_indentation = ref 0

let iprintf oc fmt =
  for i = 1 to !current_indentation do output_char oc ' ' done;
  fprintf oc fmt

let increase_indent() =
  current_indentation := !current_indentation + 2

let decrease_indent() =
  current_indentation := !current_indentation - 2

(* Remove a file, ignoring errors *)

let remove_file name =
  try Sys.remove name with Sys_error _ -> ()

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
  remove_file !temp_file

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

(* List hacking *)

let rec list_filter pred = function
    [] -> []
  | hd :: tl ->
      if pred hd then hd :: list_filter pred tl else list_filter pred tl

let rec list_partition pred = function
    [] -> ([], [])
  | hd :: tl ->
      let (p1, p2) = list_partition pred tl in
      if pred hd then (hd :: p1, p2) else (p1, hd :: p2)

let rec map_index f i = function
    [] -> []
  | hd :: tl -> f i hd :: map_index f (i + 1) tl

let rec iter_index f i = function
    [] -> ()
  | hd :: tl -> f i hd; iter_index f (i + 1) tl

(* Path searching *)

let find_in_path path name =
  if not (Filename.is_implicit name) then
    if Sys.file_exists name then name else raise Not_found
  else begin
    let rec try_dir = function
      [] -> raise Not_found
    | dir::rem ->
        let fullname = Filename.concat dir name in
        if Sys.file_exists fullname then fullname else try_dir rem
    in try_dir path
  end

(* Discard result *)

external ignore: 'a -> unit = "%identity" (* not quite *)


(***********************************************************************)
(*                                                                     *)
(*                              CamlIDL                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(***********************************************************************)

(* $Id: clflags.ml,v 1.3 1999-02-19 14:33:26 xleroy Exp $ *)

(* Command-line flags *)

let search_path = ref [Filename.current_dir_name] (* -I *)
let include_header = ref true   (* -no-include *)
let gen_header = ref false      (* -make-header *)
let prepro_defines = ref ["CAMLIDL"] (* -D *)
let use_cpp = ref true (* -cpp / -nocpp *)
let preprocessor = ref Config.cpp (* -prepro *)


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

(* $Id: enumdecl.ml,v 1.10 2000-08-18 11:23:03 xleroy Exp $ *)

(* Generation of converters for enums *)

open Printf
open Utils
open Variables
open Idltypes
open Cvttyp
open Cvtval
open Enum

(* Convert an IDL enum declaration to an ML datatype declaration *)

let ml_declaration oc en =
  if en.en_name = ""
  then fprintf oc "enum_%d =\n" en.en_stamp
  else fprintf oc "%s =\n" (String.uncapitalize en.en_name);
  List.iter
    (fun c -> fprintf oc "  | %s\n" (String.capitalize c.const_name))
    en.en_consts

(* Convert an IDL enum declaration to a C enum declaration *)

let c_declaration oc en =
  out_enum oc en; fprintf oc ";\n\n"

(* External (forward) declaration of the translation functions *)

let declare_transl oc en =
  fprintf oc "extern int camlidl_ml2c_%s_enum_%s(value);\n"
             en.en_mod en.en_name;
  fprintf oc "extern value camlidl_c2ml_%s_enum_%s(int);\n\n"
             en.en_mod en.en_name;
  fprintf oc "extern int camlidl_transl_table_%s_enum_%s[];\n\n"
             en.en_mod en.en_name

(* Translation function from an ML datatype to a C enum *)

let emit_transl_table oc en =
  fprintf oc "int camlidl_transl_table_%s_enum_%d[%d] = {\n"
             en.en_mod en.en_stamp (List.length en.en_consts);
  List.iter
    (fun c -> fprintf oc "  %s,\n" c.const_name)
    en.en_consts;
  fprintf oc "};\n\n"

let transl_ml_to_c oc en =
  current_function := sprintf "enum %s" en.en_name;
  let v = new_var "_v" in
  fprintf oc "int camlidl_ml2c_%s_enum_%s(value %s)\n"
             en.en_mod en.en_name v;
  fprintf oc "{\n";
  let pc = divert_output() in
  increase_indent();
  let c = new_c_variable (Type_int(Int, Iunboxed)) in
  enum_ml_to_c ml_to_c pc en v c;
  iprintf pc "return %s;\n" c;
  output_variable_declarations oc;
  end_diversion oc;
  decrease_indent();
  fprintf oc "}\n\n";
  current_function := ""

(* Translation function from a C enum to an ML datatype *)

let transl_c_to_ml oc en =
  current_function := sprintf "enum %s" en.en_name;
  let c = new_var "_c" in
  fprintf oc "value camlidl_c2ml_%s_enum_%s(int %s)\n"
             en.en_mod en.en_name c;
  fprintf oc "{\n";
  let pc = divert_output() in
  increase_indent();
  let v = new_ml_variable() in
  enum_c_to_ml c_to_ml pc en c v;
  iprintf pc "return %s;\n" v;
  output_variable_declarations oc;
  end_diversion oc;
  decrease_indent();
  fprintf oc "}\n\n";
  current_function := ""

(* Emit the translation functions *)

let emit_transl oc en =
  emit_transl_table oc en;
  transl_ml_to_c oc en;
  transl_c_to_ml oc en


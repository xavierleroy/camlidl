(* Generation of converters for enums *)

open Printf
open Utils
open Variables
open Idltypes
open Cvtval
open Enum

(* Convert and IDL enum declaration to an ML record declaration *)

let ml_declaration oc en =
  fprintf oc "enum_%s =\n" en.en_name;
  List.iter
    (fun c -> fprintf oc "  | %s\n" (String.capitalize c))
    en.en_consts

(* Forward declaration of the translation functions *)

let declare_transl oc en =
  fprintf oc "void _camlidl_ml2c_%s_enum_%s(value, enum %s *);\n"
             !module_name en.en_name en.en_name;
  fprintf oc "value _camlidl_c2ml_%s_enum_%s(enum %s *);\n\n"
             !module_name en.en_name en.en_name

(* Translation function from an ML datatype to a C enum *)

let transl_ml_to_c oc en =
  fprintf oc "static int _transl_table_enum_%d[%d] = {\n"
             en.en_stamp (List.length en.en_consts);
  List.iter
    (fun c -> fprintf oc "  %s,\n" c)
    en.en_consts;
  fprintf oc "};\n\n";
  current_function := sprintf "enum %s" en.en_name;
  let v = new_var "_v" in
  let c = new_var "_c" in
  fprintf oc "void _camlidl_ml2c_%s_enum_%s(value %s, int * %s)\n"
             !module_name en.en_name v (sprintf "(*%s)" c);
  let pc = divert_output() in
  enum_ml_to_c ml_to_c pc en v c;
  output_variable_declarations oc;
  end_diversion oc;
  fprintf oc "}\n\n";
  current_function := ""

(* Translation function from a C enum to an ML datatype *)

let transl_c_to_ml oc en =
  current_function := sprintf "enum %s" en.en_name;
  let c = new_var "_c" in
  fprintf oc "value _camlidl_c2ml_%s_enum_%s(int * %s)\n"
             !module_name en.en_name c;
  fprintf oc "{\n";
  let pc = divert_output() in
  let v = new_ml_variable() in
  enum_c_to_ml c_to_ml pc en (sprintf "(*%s)" c) v;
  iprintf pc "return %s;\n" v;
  output_variable_declarations oc;
  end_diversion oc;
  fprintf oc "}\n\n";
  current_function := ""

(* Emit the translation functions *)

let emit_transl oc en =
  transl_ml_to_c oc en;
  transl_c_to_ml oc en


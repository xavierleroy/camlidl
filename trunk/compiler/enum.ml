(* Handling of enums *)

open Printf
open Utils
open Variables
open Idltypes

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

let enum_ml_to_c oc en =
  fprintf oc "static enum %s _enum_%s_transl_table[%d] = {\n"
             en.en_name en.en_name (List.length en.en_consts);
  List.iter
    (fun c -> fprintf oc "  %s,\n" c)
    en.en_consts;
  fprintf oc "};\n\n";
  current_function := sprintf "enum %s" en.en_name;
  let v = new_var "_v" in
  let c = new_var "_c" in
  fprintf oc "void _camlidl_ml2c_%s_enum_%s(value %s, enum %s * %s)\n"
             !module_name en.en_name v en.en_name c;
  fprintf oc "{\n";
  fprintf oc "  *%s = _enum_%s_transl_table[Int_val(%s)];\n" c en.en_name v;
  fprintf oc "}\n\n"

(* Translation function from a C enum to an ML datatype *)

let enum_c_to_ml oc en =
  current_function := sprintf "enum %s" en.en_name;
  let c = new_var "_c" in
  fprintf oc "value _camlidl_c2ml_%s_enum_%s(enum %s * %s)\n"
             !module_name en.en_name en.en_name c;
  fprintf oc "{\n";
  if List.length en.en_consts <= 4 then begin
    fprintf oc "  switch(*%s) {\n" c;
    let rec emit_cases pos = function
      [] -> ()
    | cst :: rem ->
        fprintf oc "    case %s: return Val_int(%d);\n" cst pos;
        emit_cases (pos + 1) rem
    in emit_cases 0 en.en_consts;
    fprintf oc "    default: invalid_arg(\"bad enum %s value\");\n" en.en_name;
    fprintf oc "}\n"
  end else begin
    fprintf oc "  int _i;\n";
    fprintf oc "  for (_i = 0; _i < %d; _i++)\n" (List.length en.en_consts);
    fprintf oc "    if (*%s == _enum_%s_transl_table[_i]) return Val_int(_i);\n"
               c en.en_name;
    fprintf oc "  invalid_arg(\"bad enum %s value\");\n" en.en_name;
  end;
  fprintf oc "}\n\n"

(* Emit the translation functions *)

let emit_transl oc en =
  enum_ml_to_c oc en;
  enum_c_to_ml oc en


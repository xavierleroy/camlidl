(* Handling of typedefs *)

open Printf
open Utils
open Variables
open Idltypes
open Cvttyp
open Cvtval

(* Generate the ML type definition corresponding to the typedef *)

let ml_declaration oc td =
  if td.td_abstract then
    fprintf oc "%s\n" (String.uncapitalize td.td_name)
  else
    fprintf oc "%s = %a\n"
            (String.uncapitalize td.td_name) out_ml_type td.td_type

(* Forward declaration of the translation functions *)

let declare_transl oc td =
  fprintf oc "void _camlidl_ml2c_%s_%s(value, %s *);\n"
             !module_name td.td_name td.td_name;
  fprintf oc "value _camlidl_c2ml_%s_%s(%s *);\n"
             !module_name td.td_name td.td_name

(* Translation function from the ML type to the C type *)

let transl_ml_to_c oc td =
  current_function := sprintf "typedef %s" td.td_name;
  let v = new_var "_v" in
  let c = new_var "_c" in
  fprintf oc "void _camlidl_ml2c_%s_%s(value %s, %s * %s)\n"
             !module_name td.td_name v td.td_name c;
  fprintf oc "{\n";
  let pc = divert_output() in
  if td.td_abstract then begin
    iprintf pc "*%s = *((%a *) Bp_val(%s));\n" c out_c_type td.td_type v
  end else begin
    ml_to_c pc "_badprefix." td.td_type v (sprintf "(*%s)" c);
  end;
  output_variable_declarations oc;
  end_diversion oc;
  fprintf oc "}\n\n";
  current_function := ""

(* Translation function from the C type to the ML type *)

let transl_c_to_ml oc td =
  current_function := sprintf "typedef %s" td.td_name;
  let v = new_ml_variable() in
  let c = new_var "_c" in
  fprintf oc "value _camlidl_c2ml_%s_%s(%s * %s)\n"
             !module_name td.td_name td.td_name c;
  fprintf oc "{\n";
  let pc = divert_output() in
  if td.td_abstract then begin
    iprintf pc "%s = alloc((sizeof(%a) + sizeof(value) - 1) / sizeof(value), \
                           Abstract_tag);\n"
            v out_c_type td.td_type;
    iprintf pc "*((%a *) Bp_val(%s)) = *%s;\n"
            out_c_type td.td_type v c
  end else begin
    c_to_ml pc "_badprefix." td.td_type (sprintf "(*%s)" c) v
  end;
  iprintf pc "return %s;\n" v;
  output_variable_declarations oc;
  end_diversion oc;
  fprintf oc "}\n\n";
  current_function := ""

(* Emit the translation functions *)

let emit_transl oc td =
  transl_ml_to_c oc td;
  transl_c_to_ml oc td



(* Handling of typedefs *)

open Printf
open Utils
open Variables
open Idltypes
open Cvttyp
open Cvtval

(* Generate the ML type definition corresponding to the typedef *)

let ml_declaration oc td =
  match td with
    {td_mltype = Some s} ->
      fprintf oc "%s = %s\n" (String.uncapitalize td.td_name) s
  | {td_abstract = true} ->
      fprintf oc "%s\n" (String.uncapitalize td.td_name)
  | _ ->
      fprintf oc "%s = %a\n"
              (String.uncapitalize td.td_name) out_ml_type td.td_type

(* Translation function from the ML type to the C type *)

let transl_ml_to_c oc td =
  current_function := sprintf "typedef %s" td.td_name;
  let v = new_var "_v" in
  let c = new_var "_c" in
  fprintf oc "void camlidl_ml2c_%s_%s(value %s, %s * %s)\n"
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
  fprintf oc "value camlidl_c2ml_%s_%s(%s * %s)\n"
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
  begin match td.td_ml2c with
    Some s ->
      fprintf oc "#define camlidl_ml2c_%s_%s %s\n\n" !module_name td.td_name s
  | None ->
      transl_ml_to_c oc td
  end;
  begin match td.td_c2ml with
    Some s ->
      fprintf oc "#define camlidl_c2ml_%s_%s %s\n\n" !module_name td.td_name s
  | None ->
      transl_c_to_ml oc td
  end



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
  match td.td_type with
    Type_struct s when not td.td_abstract ->
      fprintf oc "#define _camlidl_ml2c_%s_%s _camlidl_ml2c_%s_struct_%s\n"
                 !module_name td.td_name !module_name s;
      fprintf oc "#define _camlidl_c2ml_%s_%s _camlidl_c2ml_%s_struct_%s\n"
                 !module_name td.td_name !module_name s
  | Type_named s when not td.td_abstract ->
      fprintf oc "#define _camlidl_ml2c_%s_%s _camlidl_ml2c_%s_%s\n"
                 !module_name td.td_name !module_name s;
      fprintf oc "#define _camlidl_c2ml_%s_%s _camlidl_c2ml_%s_%s\n"
                 !module_name td.td_name !module_name s
  | Type_enum s when not td.td_abstract ->
      fprintf oc "#define _camlidl_ml2c_%s_%s _camlidl_ml2c_%s_enum_%s\n"
                 !module_name td.td_name !module_name s;
      fprintf oc "#define _camlidl_c2ml_%s_%s _camlidl_c2ml_%s_enum_%s\n"
                 !module_name td.td_name !module_name s
  | Type_union(s, discr) when not td.td_abstract ->
      error
        (sprintf "typedef %s is an union; this is not supported" td.td_name)
  | _ ->
      fprintf oc "void _camlidl_ml2c_%s_%s(value, %s *);\n"
                 !module_name td.td_name td.td_name;
      fprintf oc "value _camlidl_c2ml_%s_%s(%s *);\n"
                 !module_name td.td_name td.td_name

(* Translation function from the ML type to the C type *)

let typedef_ml_to_c oc td =
  let v = new_var "_v" in
  let c = new_var "_c" in
  fprintf oc "void _camlidl_ml2c_%s_%s(value %s, %s * %s)\n"
             !module_name td.td_name v td.td_name c;
  fprintf oc "{\n";
  let pc = divert_output() in
  if td.td_abstract then begin
    iprintf pc "*%s = *((%a *) Bp_val(%s));\n" c out_c_type td.td_type v
  end else begin
    ml_to_c pc "_badprefix." td.td_type v (sprintf "*%s" c);
  end;
  output_variable_declarations oc;
  end_diversion oc;
  fprintf oc "}\n\n"

(* Translation function from the C type to the ML type *)

let typedef_c_to_ml oc td =
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
    c_to_ml pc "_badprefix." td.td_type (sprintf "*%s" c) v
  end;
  iprintf pc "return %s;\n" v;
  output_variable_declarations oc;
  end_diversion oc;
  fprintf oc "}\n\n"

(* Emit the translation functions *)

let is_trivial_typedef = function
    Type_struct s -> true
  | Type_enum s -> true
  | Type_named s -> true
  | Type_union(s,d) -> assert false (* caught earlier *)
  | _ -> false

let emit_transl oc td =
  if td.td_abstract || not(is_trivial_typedef td.td_type)
  then begin typedef_ml_to_c oc td; typedef_c_to_ml oc td end


(* Handling of typedefs *)

open Printf
open Utils
open Variables
open Idltypes
open Cvttyp
open Cvtval

(* Generate the ML type definition corresponding to the typedef *)

let declare_ml_type_abbrev oc td =
  fprintf oc "%s = %a\n" td.td_name out_ml_type td.td_type

(* Forward declaration of the translation functions *)

let declare_typedef_transl oc td =
  match td.td_type with
    Type_struct s ->
      fprintf oc "#define _camlidl_ml2c_%s_%s _camlidl_ml2c_%s_struct_%s\n"
                 !module_name td.td_name !module_name s;
      fprintf oc "#define _camlidl_c2ml_%s_%s _camlidl_c2ml_%s_struct_%s\n"
                 !module_name td.td_name !module_name s
  | Type_named s ->
      fprintf oc "#define _camlidl_ml2c_%s_%s _camlidl_ml2c_%s_%s\n"
                 !module_name td.td_name !module_name s;
      fprintf oc "#define _camlidl_c2ml_%s_%s _camlidl_c2ml_%s_%s\n"
                 !module_name td.td_name !module_name s
  | Type_enum s ->
      fprintf oc "#define _camlidl_ml2c_%s_%s _camlidl_ml2c_%s_enum_%s\n"
                 !module_name td.td_name !module_name s;
      fprintf oc "#define _camlidl_c2ml_%s_%s _camlidl_c2ml_%s_enum_%s\n"
                 !module_name td.td_name !module_name s
  | Type_union(s, discr) ->
      error
        (sprintf "typedef %s is an union; this is not supported" td.td_name)
  | _ ->
      fprintf oc "void _camlidl_ml2c_%s_%s(value, %s *);\n"
                 !module_name td.td_name td.td_name;
      fprintf oc "value _camlidl_c2ml_%s_%s(%s *)\n"
                 !module_name td.td_name td.td_name

(* Translation function from the ML type to the C type *)

let typedef_ml_to_c oc td =
  match td.td_type with
    Type_struct s -> ()
  | Type_named s -> ()
  | Type_enum s -> ()
  | ty ->
      let v = new_ml_variable() in
      let c = new_c_variable td.td_type in
      fprintf oc "void _camlidl_ml2c_%s_%s(value %s, %s * %s)\n"
                 !module_name td.td_name v td.td_name c;
      fprintf oc "{\n";
      let pc = divert_output() in
      ml_to_c pc "_badprefix." td.td_type v (sprintf "*%s" c);
      output_variable_declarations oc;
      end_diversion oc;
      fprintf oc "}\n\n"

(* Translation function from the C type to the ML type *)

let typedef_c_to_ml oc td =
  match td.td_type with
    Type_struct s -> ()
  | Type_named s -> ()
  | Type_enum s -> ()
  | ty ->
      let v = new_ml_variable() in
      let c = new_c_variable td.td_type in
      fprintf oc "value _camlidl_c2ml_%s_%s(%s * %s)\n"
                 !module_name td.td_name td.td_name c;
      fprintf oc "{\n";
      let pc = divert_output() in
      c_to_ml pc "_badprefix." td.td_type (sprintf "*%s" c) v;
      iprintf pc "return %s;\n" v;
      output_variable_declarations oc;
      end_diversion oc;
      fprintf oc "}\n\n"


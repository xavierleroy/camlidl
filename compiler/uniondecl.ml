(* Handling of union declarations *)

open Printf
open Utils
open Variables
open Idltypes
open Cvttyp
open Cvtval
open Union

(* Convert an IDL union declaration to an ML datatype declaration *)

let ml_declaration oc ud =
  if ud.ud_name = ""
  then fprintf oc "union_%d = {\n" ud.ud_stamp
  else fprintf oc "union_%s = {\n" ud.ud_name;
  let out_constr oc c =
    let s =
      if ud.ud_name <> "" then sprintf "%s_%s" ud.ud_name c
      else if c = "default" then sprintf "union_%d_default" ud.ud_stamp
      else c in
    output_string oc (String.capitalize s) in
  let emit_case = function
    {case_label = None; case_field = None} -> (* default case, no arg *)
      fprintf oc "  | %a of int\n" out_constr "default"
  | {case_label = None; case_field = Some f} -> (* default case, one arg *)
      fprintf oc "  | %a of int * %a\n" 
                 out_constr "default" out_ml_type f.field_typ
  | {case_label = Some lbl; case_field = None} -> (* named cases, no args *)
      fprintf oc "  | %a\n" out_constr lbl
  | {case_label = Some lbl; case_field = Some f} -> (* named cases, one arg *)
      fprintf oc "  | %a of %a\n" out_constr lbl out_ml_type f.field_typ in
  List.iter emit_case ud.ud_cases

(* Forward declaration of the translation functions *)

let declare_transl oc ud =
  fprintf oc "int _camlidl_ml2c_%s_union_%s(value, union %s *);\n"
             !module_name ud.ud_name ud.ud_name;
  fprintf oc "value _camlidl_c2ml_%s_union_%s(int, union %s *);\n\n"
             !module_name ud.ud_name ud.ud_name

(* Translation function from an ML datatype to a C union *)

let transl_ml_to_c oc ud =
  current_function := sprintf "union %s" ud.ud_name;
  let v = new_var "_v" in
  let c = new_var "_c" in
  fprintf oc "int _camlidl_ml2c_%s_union_%s(value %s, union %s * %s)\n"
             !module_name ud.ud_name v ud.ud_name c;
  fprintf oc "{\n";
  let pc = divert_output() in
  let discr = new_c_variable (Type_int Int) in
  iprintf pc "%s = -1;\n" discr; (* keeps gcc happy *)
  union_ml_to_c ml_to_c pc ud v (sprintf "(*%s)" c) discr;
  iprintf pc "return %s;\n" discr;
  output_variable_declarations oc;
  end_diversion oc;
  fprintf oc "}\n\n";
  check_no_deallocates "union";
  current_function := ""

(* Translation function from a C union to an ML datatype *)

let transl_c_to_ml oc ud =
  current_function := sprintf "union %s" ud.ud_name;
  let discr = new_var "_discr" in
  let c = new_var "_c" in
  fprintf oc "value _camlidl_c2ml_%s_union_%s(int %s, union %s * %s)\n"
             !module_name ud.ud_name discr ud.ud_name c;
  fprintf oc "{\n";
  let pc = divert_output() in
  let v = new_ml_variable() in
  union_c_to_ml c_to_ml pc ud (sprintf "(*%s)" c) v discr;
  output_variable_declarations oc;
  end_diversion oc;
  fprintf oc "}\n\n";
  current_function := ""

(* Emit the translation functions *)

let emit_transl oc ud =
  transl_ml_to_c oc ud;
  transl_c_to_ml oc ud

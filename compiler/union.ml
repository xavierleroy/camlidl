(* Handling of unions *)

open Printf
open Utils
open Variables
open Idltypes
open Cvttyp
open Cvtval

(* Convert an IDL union declaration to an ML datatype declaration *)

let ml_declaration oc ud =
  fprintf oc "union_%s =\n" (String.uncapitalize ud.ud_name);
  let pref = String.capitalize ud.ud_name in
  let emit_case = function
    {case_labels = []; case_field = None} -> (* default case, no arg *)
      fprintf oc "  | %s_default of int\n" pref
  | {case_labels = []; case_field = Some f} -> (* default case, one arg *)
      fprintf oc "  | %s_default of int * %a\n" pref out_ml_type f.field_typ
  | {case_labels = lbls; case_field = None} -> (* named cases, no args *)
      List.iter (fun lbl -> fprintf oc "  | %s_%s\n" pref lbl) lbls
  | {case_labels = lbls; case_field = Some f} -> (* named cases, one arg *)
      List.iter
        (fun lbl -> fprintf oc "  | %s_%s of %a\n"
                               pref lbl out_ml_type f.field_typ)
        lbls in
  List.iter emit_case ud.ud_cases

(* Forward declaration of the translation functions *)

let declare_transl oc ud =
  fprintf oc "void _camlidl_ml2c_%s_union_%s(value, union %s *);\n"
             !module_name ud.ud_name ud.ud_name;
  fprintf oc "value _camlidl_c2ml_%s_union_%s(int, union %s *);\n\n"
             !module_name ud.ud_name ud.ud_name

(* Translation function from an ML datatype to a C union *)

let union_ml_to_c oc ud =
  current_function := sprintf "union %s" ud.ud_name;
  let v = new_var "_v" in
  let c = new_var "_c" in
  fprintf oc "void _camlidl_ml2c_%s_union_%s(value %s, union %s * %s)\n"
             !module_name ud.ud_name v ud.ud_name c;
  fprintf oc "{\n";
  let pc = divert_output() in
  let tag = ref 0 in
  let emit_case case =
    match case.case_field with
      None -> ()
    | Some { field_name = n; field_typ = ty } ->
        if case.case_labels = [] then begin
          iprintf pc "case %d: /* default */\n" !tag;
          incr tag
        end else begin
          List.iter
            (fun lbl ->
              iprintf pc "case %d: /* %s */\n" !tag lbl;
              incr tag)
            case.case_labels;
        end;
        increase_indent();
        let v' = new_ml_variable() in
        iprintf pc "%s = Field(%s, 0);\n" v' v;
        ml_to_c pc "_badprefix." ty v' (sprintf "%s->%s" c n);
        iprintf pc "break;\n";
        decrease_indent() in
  iprintf pc "if (Is_long(%s)) return;\n" v;
  iprintf pc "switch (Tag_val(%s)) {\n" v;
  List.iter emit_case ud.ud_cases;
  iprintf pc "}\n";
  output_variable_declarations oc;
  end_diversion oc;
  fprintf oc "}\n\n";
  current_function := ""

(* Translation function from a C union to an ML datatype *)

let union_c_to_ml oc ud =
  current_function := sprintf "union %s" ud.ud_name;
  let discr = new_var "_discr" in
  let c = new_var "_c" in
  fprintf oc "value _camlidl_c2ml_%s_union_%s(int %s, union %s * %s)\n"
             !module_name ud.ud_name discr ud.ud_name c;
  fprintf oc "{\n";
  let pc = divert_output() in
  iprintf pc "switch (%s) {\n" discr;
  let tag_const = ref 0 and tag_arg = ref 0 in
  let emit_case = function
    { case_labels = []; case_field = None } -> (* default case, no arg *)
      iprintf pc "default:\n";
      increase_indent();
      let v = new_ml_variable() in
      iprintf pc "%s = alloc_small(1, %d);\n" v !tag_arg;
      incr tag_arg;
      iprintf pc "Field(%s, 0) = Val_int(%s);\n" v discr;
      iprintf pc "return %s;\n" v;
      decrease_indent()
  | { case_labels = []; case_field = Some{field_name = n; field_typ = ty}} ->
                                               (* default case, one arg *)
      iprintf pc "default:\n";
      increase_indent();
      let v' = new_ml_variable() in
      c_to_ml pc "_badprefix." ty (sprintf "%s->%s" c n) v';
      let v = new_ml_variable() in
      iprintf pc "Begin_root(%s)\n" v';
      increase_indent();
      iprintf pc "%s = alloc_small(2, %d);\n" v !tag_arg;
      incr tag_arg;
      iprintf pc "Field(%s, 0) = Val_int(%s);\n" v discr;
      iprintf pc "Field(%s, 1) = %s;\n" v v';
      decrease_indent();
      iprintf pc "End_roots()\n";
      iprintf pc "return %s;\n" v;
      decrease_indent()
  | { case_labels = lbls; case_field = None } -> (* named cases, no arg *)
      List.iter
        (fun lbl ->
          iprintf pc "case %s:\n" lbl;
          iprintf pc "  return Val_int(%d);\n" !tag_const;
          incr tag_const)
        lbls
  | { case_labels = lbls; case_field = Some{field_name = n; field_typ = ty}} ->
                                                  (* named cases, one arg *)
      let emit_label lbl =
        iprintf pc "case %s:\n" lbl;
        increase_indent();
        let v' = new_ml_variable() in
        c_to_ml pc "_badprefix." ty (sprintf "%s->%s" c n) v';
        let v = new_ml_variable() in
        iprintf pc "Begin_root(%s)\n" v';
        increase_indent();
        iprintf pc "%s = alloc_small(1, %d);\n" v !tag_arg;
        incr tag_arg;
        iprintf pc "Field(%s, 0) = %s;\n" v v';
        decrease_indent();
        iprintf pc "End_roots()\n";
        iprintf pc "return %s;\n" v;
        decrease_indent() in
      List.iter emit_label lbls in
  List.iter emit_case ud.ud_cases;
  iprintf pc "default:\n";
  iprintf pc "  invalid_arg(\"bad discriminant for union %s\");\n" ud.ud_name;
  iprintf pc "}\n";
  output_variable_declarations oc;
  end_diversion oc;
  fprintf oc "}\n\n";
  current_function := ""

(* Emit the translation functions *)

let emit_transl oc ud =
  union_ml_to_c oc ud;
  union_c_to_ml oc ud

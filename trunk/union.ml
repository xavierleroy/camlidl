(* Handling of unions *)

open Printf
open Utils
open Variables
open Idltypes
open Cvttyp

(* Translate an ML datatype [v] and store its argument in the C union [c]
   and its discriminant in the C integer [discr]. *)

let union_ml_to_c ml_to_c oc ud v c discr =
  let tag_constant = ref 0
  and tag_constr = ref 0 in
  let emit_case = function
    {case_field = None; case_label = None} -> (* default case, no arg *)
      iprintf oc "case %d: /* default */\n" !tag_constr;
      incr tag_constr;
      iprintf oc "  %s = Int_val(Field(%s, 0));\n" discr v;
      iprintf oc "  break;\n"
  | {case_field = Some{field_name = n; field_typ = ty}; case_label = None} ->
      (* default case, one arg *)
      iprintf oc "case %d: /* default */\n" !tag_constr;
      incr tag_constr;
      increase_indent();
      iprintf oc "%s = Int_val(Field(%s, 0));\n" discr v;
      let v' = new_ml_variable() in
      iprintf oc "%s = Field(%s, 1);\n" v' v;
      ml_to_c oc "_badprefix." ty v' (sprintf "%s.%s" c n);
      iprintf oc "break;\n";
      decrease_indent()
  | {case_field = None; case_label = Some lbl} -> (* named case, no args *)
      iprintf oc "case %d: /* %s */\n" !tag_constant lbl;
      incr tag_constant;
      iprintf oc "  %s = %s;\n" discr lbl;
      iprintf oc "  break;\n"
  | {case_field = Some{field_name = n; field_typ = ty};
     case_label = Some lbl} ->          (* named case, one arg *)
      iprintf oc "case %d: /* %s */\n" !tag_constr lbl;
      incr tag_constr;
      increase_indent();
      iprintf oc "%s = %s;\n" discr lbl;
      let v' = new_ml_variable() in
      iprintf oc "%s = Field(%s, 0);\n" v' v;
      ml_to_c oc "_badprefix." ty v' (sprintf "%s.%s" c n);
      iprintf oc "break;\n";
      decrease_indent() in
  let (constant_cases, constr_cases) =
    list_partition
      (fun c -> c.case_field = None && c.case_label <> None)
      ud.ud_cases in
  if constant_cases <> [] && constr_cases <> [] then begin
    iprintf oc "if (Is_long(%s)) {\n" v;
    increase_indent()
  end;
  if constant_cases <> [] then begin
    iprintf oc "switch (Int_val(%s)) {\n" v;
    List.iter emit_case constant_cases;
    iprintf oc "}\n"
  end;
  if constant_cases <> [] && constr_cases <> [] then begin
    decrease_indent();
    iprintf oc "} else {\n";
    increase_indent()
  end;
  if constr_cases <> [] then begin
    iprintf oc "switch (Tag_val(%s)) {\n" v;
    List.iter emit_case constr_cases;
    iprintf oc "}\n"
  end;
  if constant_cases <> [] && constr_cases <> [] then begin
    decrease_indent();
    iprintf oc "}\n"
  end

(* Translate a C union [c] with its discriminant [discr]
   to an ML datatype [v]. *)

let union_c_to_ml c_to_ml oc ud c v discr =
  let tag_constant = ref 0 and tag_constr = ref 0 in
  let have_default = ref false in
  let emit_case = function
    {case_field = None; case_label = None} -> (* default case, no arg *)
      iprintf oc "default:\n";
      increase_indent();
      iprintf oc "%s = camlidl_alloc_small(1, %d);\n" v !tag_constr;
      incr tag_constr;
      iprintf oc "Field(%s, 0) = Val_int(%s);\n" v discr;
      decrease_indent();
      have_default := true
  | {case_field = Some{field_name = n; field_typ = ty}; case_label = None} ->
      (* default case, one arg *)
      iprintf oc "default:\n";
      increase_indent();
      let v' = new_ml_variable() in
      c_to_ml oc "_badprefix." ty (sprintf "%s.%s" c n) v';
      iprintf oc "Begin_root(%s)\n" v';
      increase_indent();
      iprintf oc "%s = camlidl_alloc_small(2, %d);\n" v !tag_constr;
      incr tag_constr;
      iprintf oc "Field(%s, 0) = Val_int(%s);\n" v discr;
      iprintf oc "Field(%s, 1) = %s;\n" v v';
      decrease_indent();
      iprintf oc "End_roots()\n";
      decrease_indent();
      have_default := true
  | {case_field = None; case_label = Some lbl} -> (* named cases, no arg *)
      iprintf oc "case %s:\n" lbl;
      iprintf oc "  %s = Val_int(%d);\n" v !tag_constant;
      incr tag_constant
  | {case_field = Some{field_name = n; field_typ = ty};
     case_label = Some lbl} ->                    (* named cases, one arg *)
      iprintf oc "case %s:\n" lbl;
      increase_indent();
      let v' = new_ml_variable() in
      c_to_ml oc "_badprefix." ty (sprintf "%s.%s" c n) v';
      iprintf oc "Begin_root(%s)\n" v';
      increase_indent();
      iprintf oc "%s = camlidl_alloc_small(1, %d);\n" v !tag_constr;
      incr tag_constr;
      iprintf oc "Field(%s, 0) = %s;\n" v v';
      decrease_indent();
      iprintf oc "End_roots()\n";
      decrease_indent() in
  iprintf oc "switch (%s) {\n" discr;
  List.iter emit_case ud.ud_cases;
  if not !have_default then begin
    iprintf oc "default:\n";
    iprintf oc "  invalid_argument(\"%s: bad discriminant for union %s\");\n" 
               !current_function ud.ud_name
  end;
  iprintf oc "}\n"

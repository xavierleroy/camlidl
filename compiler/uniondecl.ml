(***********************************************************************)
(*                                                                     *)
(*                              CamlIDL                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Lesser General Public License LGPL v2.1 *)
(*                                                                     *)
(***********************************************************************)

(* $Id: uniondecl.ml,v 1.16 2002-01-16 09:42:04 xleroy Exp $ *)

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
  then fprintf oc "union_%d =\n" ud.ud_stamp
  else fprintf oc "%s =\n" (String.uncapitalize_ascii ud.ud_name);
  let out_constr oc c =
    if c = "default" then
      if ud.ud_name <> ""
      then fprintf oc "Default_%s" ud.ud_name
      else fprintf oc "Default_%d" ud.ud_stamp
    else
      output_string oc (String.capitalize_ascii c) in
  let emit_case = function
    {case_labels = []; case_field = None} -> (* default case, no arg *)
      fprintf oc "  | %a of int\n" out_constr "default"
  | {case_labels = []; case_field = Some f} -> (* default case, one arg *)
      fprintf oc "  | %a of int * %a\n"
                 out_constr "default" out_ml_type f.field_typ
  | {case_labels = lbls; case_field = None} -> (* named cases, no args *)
      List.iter
        (fun lbl -> fprintf oc "  | %a\n" out_constr lbl)
        lbls
  | {case_labels = lbls; case_field = Some f} -> (* named cases, one arg *)
      List.iter
        (fun lbl ->
          fprintf oc "  | %a of %a\n" out_constr lbl out_ml_type f.field_typ)
        lbls in
  List.iter emit_case ud.ud_cases

(* Convert an IDL union declaration to a C union declaration *)

let c_declaration oc ud =
  if ud.ud_cases = []
  then fprintf oc "union %s;\n" ud.ud_name
  else begin out_union oc ud; fprintf oc ";\n\n" end

(* External (forward) declaration of the translation functions *)

let declare_transl oc ud =
  fprintf oc "extern int camlidl_ml2c_%s_union_%s(value, union %s *, camlidl_ctx _ctx);\n"
             ud.ud_mod ud.ud_name ud.ud_name;
  fprintf oc "extern value camlidl_c2ml_%s_union_%s(int, union %s *, camlidl_ctx _ctx);\n\n"
             ud.ud_mod ud.ud_name ud.ud_name

(* Translation function from an ML datatype to a C union *)

let transl_ml_to_c oc ud =
  current_function := sprintf "union %s" ud.ud_name;
  let v = new_var "_v" in
  let c = new_var "_c" in
  fprintf oc "int camlidl_ml2c_%s_union_%s(value %s, union %s * %s, camlidl_ctx _ctx)\n"
             ud.ud_mod ud.ud_name v ud.ud_name c;
  fprintf oc "{\n";
  let pc = divert_output() in
  increase_indent();
  let discr = new_c_variable (Type_int(Int, Iunboxed)) in
  iprintf pc "%s = -1;\n" discr; (* keeps gcc happy *)
  union_ml_to_c ml_to_c pc false Prefix.empty ud v (sprintf "(*%s)" c) discr;
  iprintf pc "return %s;\n" discr;
  output_variable_declarations oc;
  end_diversion oc;
  decrease_indent();
  fprintf oc "}\n\n";
  current_function := ""

(* Translation function from a C union to an ML datatype *)

let transl_c_to_ml oc ud =
  current_function := sprintf "union %s" ud.ud_name;
  let discr = new_var "_discr" in
  let c = new_var "_c" in
  fprintf oc "value camlidl_c2ml_%s_union_%s(int %s, union %s * %s, camlidl_ctx _ctx)\n"
             ud.ud_mod ud.ud_name discr ud.ud_name c;
  fprintf oc "{\n";
  let pc = divert_output() in
  increase_indent();
  let v = new_ml_variable() in
  union_c_to_ml c_to_ml pc Prefix.empty ud (sprintf "(*%s)" c) v discr;
  iprintf pc "return %s;\n" v;
  output_variable_declarations oc;
  end_diversion oc;
  decrease_indent();
  fprintf oc "}\n\n";
  current_function := ""

(* Emit the translation functions *)

let emit_transl oc ud =
  transl_ml_to_c oc ud;
  transl_c_to_ml oc ud

(* Generate the ML interface *)

open Printf
open Utils
open Idltypes

let gen_ml_decls oc intf all_type_decls =
  fprintf oc "(* File generated from %s.idl *)\n\n" !module_name;
  (* Generate the type definitions *)
  let first = ref true in
  let start_decl () =
    if !first then fprintf oc "type " else fprintf oc "and ";
    first := false in
  let emit_typedef = function
      Comp_typedecl td -> start_decl(); Typedef.ml_declaration oc td
    | Comp_structdecl s -> start_decl(); Structdecl.ml_declaration oc s
    | Comp_uniondecl u -> start_decl(); Uniondecl.ml_declaration oc u
    | Comp_enumdecl e -> start_decl(); Enumdecl.ml_declaration oc e
    | _ -> () in
  List.iter emit_typedef all_type_decls;
  fprintf oc "\n";
  (* Generate the function declarations *)
  let emit_fundecl = function
      Comp_fundecl fd -> Funct.ml_declaration oc fd
    | Comp_constdecl cd -> Constdecl.ml_declaration oc cd
    | Comp_diversion(Div_ml, txt) -> output_string oc txt
    | _ -> () in
  List.iter emit_fundecl intf

(* Handling of interfaces *)

open Printf
open Utils
open Idltypes

type diversion_type = Div_c | Div_ml

type component =
    Comp_typedecl of Typedef.type_decl
  | Comp_structdecl of struct_decl
  | Comp_uniondecl of union_decl
  | Comp_enumdecl of enum_decl
  | Comp_fundecl of Funct.function_decl
  | Comp_constdecl of Constdecl.constant_decl
  | Comp_diversion of diversion_type * string

type idl_file = component list

(* Generate the ML interface *)

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

(* Generate the C stub file *)

(* Process a component *)

let process_comp oc = function
    Comp_typedecl td ->
      Typedef.emit_transl oc td
  | Comp_structdecl sd ->
      if sd.sd_fields = []
      then Structdecl.declare_transl oc sd
      else Structdecl.emit_transl oc sd
  | Comp_uniondecl ud ->
      if ud.ud_cases = []
      then Uniondecl.declare_transl oc ud
      else Uniondecl.emit_transl oc ud
  | Comp_enumdecl en ->
      if en.en_consts = []
      then Enumdecl.declare_transl oc en
      else Enumdecl.emit_transl oc en
  | Comp_fundecl fd ->
      Funct.emit_wrapper oc fd
  | Comp_constdecl cd ->
      ()
  | Comp_diversion(kind, txt) ->
      if kind = Div_c then output_string oc txt

let gen_c_stub oc intf =
  (* Output the header *)
  fprintf oc "/* File generated from %s.idl */\n\n" !module_name;
  output_string oc "\
    #include <stddef.h>\n\
    #include <string.h>\n\
    #include <caml/mlvalues.h>\n\
    #include <caml/memory.h>\n\
    #include <caml/alloc.h>\n\
    #include <caml/fail.h>\n\
    #include <caml/camlidlruntime.h>\n\n";
  (* Process the interface *)
  List.iter (process_comp oc) intf

(* Generate the ML interface *)

open Printf
open Utils
open Idltypes

let gen_ml_decls oc intf =
  (* Generate the type definitions *)
  let first = ref true in
  let start_decl () =
    if !first then fprintf oc "type " else fprintf oc "and ";
    first := false in
  List.iter
    (function
        Comp_typedecl tdl ->
          List.iter
            (fun td -> start_decl(); Typedef.ml_declaration oc td)
            tdl
      | Comp_structdecl sd ->
          start_decl(); Struct.ml_declaration oc sd
      | Comp_enumdecl en ->
          start_decl(); Enum.ml_declaration oc en
      | Comp_uniondecl ud ->
          start_decl(); Union.ml_declaration oc ud
      | Comp_fundecl fd -> ())
    intf;
  fprintf oc "\n";
  (* Generate the function declarations *)
  List.iter
    (function
        Comp_fundecl fd -> Funct.ml_declaration oc fd
      | _ -> ())
    intf

      

(* Generate the ML interface *)

open Printf
open Utils
open Idltypes
open Typedef
open Struct
open Funct

let gen_ml_decls oc intf =
  (* Generate the type definitions *)
  let first = ref true in
  let start_decl () =
    if !first then fprintf oc "type " else fprintf oc "and  ";
    first := false in
  List.iter
    (function
        Comp_typedecl tdl ->
          List.iter
            (fun td -> start_decl(); declare_ml_type_abbrev oc td)
            tdl
      | Comp_structdecl sd ->
          start_decl(); declare_ml_record oc sd          
      | Comp_fundecl fd -> ()
      | _ -> assert false)
    intf;
  fprintf oc "\n";
  (* Generate the function declarations *)
  List.iter
    (function
        Comp_fundecl fd -> declare_ml_function oc fd
      | _ -> ())
    intf

      

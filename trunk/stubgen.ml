(* Generate the C stub file *)

open Utils
open Idltypes
open Typedef
open Struct
open Funct

let gen_c_stub oc intf =
  (* Generate forward declarations for all conversion functions *)
  List.iter
    (function Comp_typedecl tdl -> List.iter (declare_typedef_transl oc) tdl
            | Comp_structdecl sd -> declare_struct_transl oc sd
            | Comp_fundecl fd -> ()
            | _ -> assert false)
    intf;
  (* Generate conversion functions for named types *)
  List.iter
    (function Comp_typedecl tdl ->
                List.iter
                  (fun td -> typedef_ml_to_c oc td; typedef_c_to_ml oc td)
                  tdl
            | Comp_structdecl sd ->
                struct_ml_to_c oc sd;
                struct_c_to_ml oc sd
            | Comp_fundecl fd -> ()
            | _ -> assert false)
    intf;
  (* Generate stub code for functions *)
  List.iter
    (function Comp_fundecl fd -> function_wrapper oc fd
            | _ -> ())
    intf



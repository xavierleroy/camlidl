(* Generate the C stub file *)

open Printf
open Utils
open Idltypes

let gen_c_stub oc incl intf =
  (* Output the header *)
  fprintf oc "/* File generated from %s.idl */\n\n" !module_name;
  output_string oc "\
    #include <stddef.h>\n\
    #include <caml/mlvalues.h>\n\
    #include <caml/memory.h>\n\
    #include <caml/alloc.h>\n\
    #include <caml/fail.h>\n\n";
  output_string oc incl;
  (* Generate forward declarations for all conversion functions *)
  List.iter
    (function Comp_typedecl td -> Typedef.declare_transl oc td
            | Comp_structdecl sd -> Structdecl.declare_transl oc sd
            | Comp_uniondecl ud -> Uniondecl.declare_transl oc ud
            | Comp_enumdecl en -> Enumdecl.declare_transl oc en
            | Comp_fundecl fd -> ())
    intf;
  (* Generate conversion functions for named types *)
  List.iter
    (function Comp_typedecl td -> Typedef.emit_transl oc td
            | Comp_structdecl sd -> Structdecl.emit_transl oc sd
            | Comp_uniondecl ud -> Uniondecl.emit_transl oc ud
            | Comp_enumdecl en -> Enumdecl.emit_transl oc en
            | Comp_fundecl fd -> ())
    intf;
  (* Generate stub code for functions *)
  List.iter
    (function Comp_fundecl fd -> Funct.emit_wrapper oc fd
            | _ -> ())
    intf



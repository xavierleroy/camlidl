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
    (function Comp_typedecl tdl -> List.iter (Typedef.declare_transl oc) tdl
            | Comp_structdecl sd -> Struct.declare_transl oc sd
            | Comp_uniondecl ud -> Union.declare_transl oc ud
            | Comp_enumdecl en -> Enum.declare_transl oc en
            | Comp_fundecl fd -> ())
    intf;
  (* Generate conversion functions for named types *)
  List.iter
    (function Comp_typedecl tdl -> List.iter (Typedef.emit_transl oc) tdl
            | Comp_structdecl sd -> Struct.emit_transl oc sd
            | Comp_uniondecl ud -> Union.emit_transl oc ud
            | Comp_enumdecl en -> Enum.emit_transl oc en
            | Comp_fundecl fd -> ())
    intf;
  (* Generate stub code for functions *)
  List.iter
    (function Comp_fundecl fd -> Funct.emit_wrapper oc fd
            | _ -> ())
    intf



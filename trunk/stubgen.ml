(* Generate the C stub file *)

open Printf
open Utils
open Idltypes

(* Process an interface component *)

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
  | Comp_diversion txt ->
      output_string oc txt

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

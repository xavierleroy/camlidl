(* Handling of COM-style interfaces *)

open Idltypes
open Funct

type interface =
  { intf_name: string;                  (* Name of interface *)
    intf_super: interface option;       (* Super-interface, if any *)
    intf_methods: function_decl list;   (* Methods *)
    intf_uid: string }                  (* Unique interface ID *)

val ml_class_declaration: output_channel -> interface -> unit
val ml_class_definition: output_channel -> interface -> unit
val declare_transl: output_channel -> interface -> unit
val emit_transl: output_channel -> interface -> unit

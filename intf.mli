(* Handling of COM-style interfaces *)

open Idltypes
open Funct

type interface =
  { intf_name: string;                  (* Name of interface *)
    intf_super: interface;              (* Super-interface *)
    intf_methods: function_decl list;   (* Methods *)
    intf_uid: string }                  (* Unique interface ID *)

val ml_declaration: out_channel -> interface -> unit
val ml_class_declaration: out_channel -> interface -> unit
val ml_class_definition: out_channel -> interface -> unit
val emit_transl: out_channel -> interface -> unit

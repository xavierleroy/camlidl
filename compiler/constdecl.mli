(* Handling of constant declarations *)

open Idltypes

type constant_value = Cst_int of int | Cst_string of string

type constant_decl =
  { cd_name: string; cd_type: idltype; cd_value: constant_value }

val ml_declaration: out_channel -> constant_decl -> unit
val ml_definition: out_channel -> constant_decl -> unit

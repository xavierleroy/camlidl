(* Handling of constant declarations *)

open Idltypes

type constant_decl =
  { cd_name: string; cd_type: idltype; cd_value: lexpr }

val ml_declaration: out_channel -> constant_decl -> unit
val ml_definition: out_channel -> constant_decl -> unit

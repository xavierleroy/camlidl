(* Generation of converters for structs *)

open Idltypes

val ml_declaration : out_channel -> struct_decl -> unit
val declare_transl: out_channel -> struct_decl -> unit
val emit_transl : out_channel -> struct_decl -> unit

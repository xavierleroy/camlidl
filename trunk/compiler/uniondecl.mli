(* Generation of converters for unions *)

open Idltypes

val ml_declaration : out_channel -> union_decl -> unit
val declare_transl: out_channel -> union_decl -> unit
val emit_transl : out_channel -> union_decl -> unit

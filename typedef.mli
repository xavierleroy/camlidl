(* Handling of typedefs *)

open Idltypes

val ml_declaration: out_channel -> type_decl -> unit
val declare_transl: out_channel -> type_decl -> unit
val emit_transl: out_channel -> type_decl -> unit

(* Generation of converters for enums *)

open Idltypes

val ml_declaration : out_channel -> enum_decl -> unit
val c_declaration : out_channel -> enum_decl -> unit
val declare_transl: out_channel -> enum_decl -> unit
val emit_transl : out_channel -> enum_decl -> unit

(* Generation of converters for structs *)

open Idltypes

val declare_ml_record : out_channel -> struct_decl -> unit
val declare_struct_transl: out_channel -> struct_decl -> unit
val struct_ml_to_c : out_channel -> struct_decl -> unit
val struct_c_to_ml : out_channel -> struct_decl -> unit

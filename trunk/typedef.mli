(* Handling of typedefs *)

open Idltypes

val declare_ml_type_abbrev: out_channel -> type_decl -> unit
val declare_typedef_transl: out_channel -> type_decl -> unit
val typedef_ml_to_c: out_channel -> type_decl -> unit
val typedef_c_to_ml: out_channel -> type_decl -> unit

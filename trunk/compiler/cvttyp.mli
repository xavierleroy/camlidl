open Idltypes

(* Convert an IDL type to a C declarator *)
val out_c_decl : out_channel -> string * Idltypes.idltype -> unit

(* Convert an IDL type to a C type *)
val out_c_type : out_channel -> Idltypes.idltype -> unit

(* Convert an IDL type to an ML type *)
val out_ml_type : out_channel -> Idltypes.idltype -> unit

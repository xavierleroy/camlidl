open Idltypes

(* Convert an IDL type to a C declarator *)
val out_c_decl : out_channel -> string * Idltypes.idltype -> unit

(* Convert an IDL type to a C type *)
val out_c_type : out_channel -> Idltypes.idltype -> unit

(* Convert an IDL type to an ML type *)
val out_ml_type : out_channel -> Idltypes.idltype -> unit

(* Output a reference to a restricted expression *)
val string_of_restr_expr: restricted_expr -> string
val out_restr_expr: out_channel -> restricted_expr -> unit

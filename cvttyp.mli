open Idltypes

(* Convert an IDL type to a C declarator *)
val out_c_decl : out_channel -> string * Idltypes.idltype -> unit

(* Convert an IDL type to a C type *)
val out_c_type : out_channel -> Idltypes.idltype -> unit

(* Convert an IDL type to an ML type *)
val out_ml_type : out_channel -> Idltypes.idltype -> unit

(* Convert a list of IDL types to an ML type *)
val out_ml_types :
      out_channel -> string -> ('a * Idltypes.idltype) list -> unit

(* Print an ML type name, qualified if necessary *)
val out_mltype_name: out_channel -> string * string -> unit

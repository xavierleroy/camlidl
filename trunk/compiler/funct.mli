(* Generation of stub code for functions *)

open Idltypes

val ml_declaration : out_channel -> function_decl -> unit

val emit_wrapper : out_channel -> function_decl -> unit

(* Generation of stub code for functions *)

open Idltypes

val declare_ml_function : out_channel -> function_decl -> unit

val function_wrapper : out_channel -> function_decl -> unit

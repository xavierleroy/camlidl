(* Generation of stub code for functions *)

open Idltypes

type in_out =
    In | Out | InOut

type function_decl =
  { fun_name: string;
    fun_res: idltype;
    fun_params: (string * in_out * idltype) list;
    fun_ccall: string option }

val ml_declaration : out_channel -> function_decl -> unit

val emit_wrapper : out_channel -> function_decl -> unit

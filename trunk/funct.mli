(* Generation of stub code for functions *)

open Idltypes

type in_out =
    In | Out | InOut

type function_decl =
  { fun_name: string;
    fun_res: idltype;
    fun_params: (string * in_out * idltype) list;
    fun_call: out_channel -> function_decl -> unit }

val ml_view :
      function_decl -> (string * idltype) list * (string * idltype) list

val ml_declaration : out_channel -> function_decl -> unit

val emit_wrapper : out_channel -> function_decl -> unit

val emit_standard_call : out_channel -> function_decl -> unit
val emit_custom_call : string -> out_channel -> function_decl -> unit

val out_inout : out_channel -> in_out -> unit

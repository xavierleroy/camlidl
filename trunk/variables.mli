(* Generate temporaries *)

val new_var : string -> string
val new_c_variable : Idltypes.idltype -> string
val new_ml_variable : unit -> string
val new_ml_variable_block : int -> string
val output_variable_declarations : out_channel -> unit
val init_value_block : out_channel -> string -> int -> unit
val copy_values_to_block : out_channel -> string -> string -> int -> unit
val need_context : bool ref

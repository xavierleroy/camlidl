(* Generate temporaries *)

val new_var : string -> string
val new_c_variable : Idltypes.idltype -> string
val new_ml_variable : unit -> string
val output_variable_declarations : out_channel -> unit
val copy_values_to_block : out_channel -> string -> string -> int -> unit
val add_to_deallocate : string -> unit
val output_deallocates : out_channel -> unit

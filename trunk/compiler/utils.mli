(* Utility functions *)

val iprintf : out_channel -> ('a, out_channel, unit) format -> 'a
val increase_indent : unit -> unit
val decrease_indent : unit -> unit

val divert_output : unit -> out_channel
val end_diversion : out_channel -> unit

val module_name : string ref
val current_function : string ref

val error : string -> 'a
exception Error


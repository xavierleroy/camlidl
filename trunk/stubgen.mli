(* Generate the C stub file *)

open Idltypes

val gen_c_stub: out_channel -> string -> interface -> unit

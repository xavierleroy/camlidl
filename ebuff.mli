(* String buffers *)

type t

val create: int -> t
val reset: t -> unit
val add_char: t -> char -> unit
val add_substring: t -> string -> int -> int -> unit
val add_string: t -> string -> unit
val get_stored: t -> string
val output: out_channel -> t -> unit

(* Conversion of values between ML and C *)

open Idltypes

val ml_to_c :
  out_channel -> string -> idltype -> string -> string -> unit
val c_to_ml :
  out_channel -> string -> idltype -> string -> string -> unit

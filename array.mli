(* Marshaling for arrays *)

open Idltypes

val array_ml_to_c : 
  (out_channel -> string -> idltype -> string -> string -> unit) ->
    out_channel -> string -> array_attributes -> idltype -> string -> string ->
      unit
val array_c_to_ml : 
  (out_channel -> string -> idltype -> string -> string -> unit) ->
    out_channel -> string -> array_attributes -> idltype -> string -> string ->
      unit


(* Marshalling for unions *)

open Idltypes

val union_ml_to_c : 
  (out_channel -> bool -> string -> idltype -> string -> string -> unit) ->
    out_channel -> bool -> union_decl -> string -> string -> string -> unit
val union_c_to_ml : 
  (out_channel -> string -> idltype -> string -> string -> unit) ->
    out_channel -> union_decl -> string -> string -> string -> unit

(* Handling of enums *)

open Idltypes

val enum_ml_to_c : 
  (out_channel -> string -> idltype -> string -> string -> unit) ->
    out_channel -> enum_decl -> string -> string -> unit
val enum_c_to_ml : 
  (out_channel -> string -> idltype -> string -> string -> unit) ->
    out_channel -> enum_decl -> string -> string -> unit


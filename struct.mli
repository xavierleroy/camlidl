(* Marshaling for structs *)

open Idltypes

val struct_ml_to_c : 
  (out_channel -> bool -> string -> idltype -> string -> string -> unit) ->
    out_channel -> bool -> struct_decl -> string -> string -> unit
val struct_c_to_ml : 
  (out_channel -> string -> idltype -> string -> string -> unit) ->
    out_channel -> struct_decl -> string -> string -> unit

val remove_dependent_fields: field list -> field list

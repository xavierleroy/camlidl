(* Normalization of IDL types after parsing *)

open Idltypes

val interface: interface -> interface * interface
  (* First result: normalized interface
     Second result: list of all type definitions, including
       anonymous structs, enums, unions. *)

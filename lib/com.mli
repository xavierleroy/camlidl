(* Run-time library for COM components *)

type 'a opaque

type 'a interface

type 'a iid

exception Error of string

val queryInterface: 'a interface -> 'b iid -> 'b interface

type iUnknown

val iUnknown_of : 'a interface -> iUnknown interface

val aggregate: 'a interface -> 'b interface -> 'a interface

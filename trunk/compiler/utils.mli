(***********************************************************************)
(*                                                                     *)
(*                              CamlIDL                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(***********************************************************************)

(* $Id: utils.mli,v 1.8 1999-02-19 14:33:43 xleroy Exp $ *)

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

val list_filter : ('a -> bool) -> 'a list -> 'a list
val list_partition : ('a -> bool) -> 'a list -> 'a list * 'a list
val map_index : (int -> 'a -> 'b) -> int -> 'a list -> 'b list
val iter_index : (int -> 'a -> unit) -> int -> 'a list -> unit

val find_in_path : string list -> string -> string

external ignore: 'a -> unit = "%identity"

val remove_file : string -> unit

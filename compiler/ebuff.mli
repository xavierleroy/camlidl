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

(* $Id: ebuff.mli,v 1.2 1999-02-19 14:33:28 xleroy Exp $ *)

(* String buffers *)

type t

val create: int -> t
val reset: t -> unit
val add_char: t -> char -> unit
val add_substring: t -> string -> int -> int -> unit
val add_string: t -> string -> unit
val get_stored: t -> string
val output: out_channel -> t -> unit

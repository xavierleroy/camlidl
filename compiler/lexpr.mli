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

(* $Id: lexpr.mli,v 1.2 1999-02-19 14:33:32 xleroy Exp $ *)

(* Evaluation and pretty-printing of limited expressions *)

open Idltypes

type constant_value = Cst_int of int | Cst_string of string

val bind_const: string -> constant_value -> unit

val eval_int: lexpr -> int
val eval: lexpr -> constant_value

val tostring: string -> lexpr -> string
val output: out_channel -> string * lexpr -> unit

val is_free: string -> lexpr -> bool
val is_dependent: string -> idltype -> bool

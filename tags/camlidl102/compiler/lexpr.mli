(***********************************************************************)
(*                                                                     *)
(*                              CamlIDL                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0                *)
(*                                                                     *)
(***********************************************************************)

(* $Id: lexpr.mli,v 1.3 2000-08-19 11:04:57 xleroy Exp $ *)

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

(***********************************************************************)
(*                                                                     *)
(*                              CamlIDL                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Lesser General Public License LGPL v2.1 *)
(*                                                                     *)
(***********************************************************************)

(* $Id: lexpr.mli,v 1.5 2002-01-16 16:15:32 xleroy Exp $ *)

(* Evaluation and pretty-printing of limited expressions *)

open Idltypes

type constant_value =
    Cst_int of int32
  | Cst_long of nativeint
  | Cst_longlong of int64
  | Cst_string of string

val bind_const: string -> constant_value -> unit

val is_true: constant_value -> bool
val int_val: constant_value -> int
val int32_val: constant_value -> int32
val nativeint_val: constant_value -> nativeint
val int64_val: constant_value -> int64
val string_val: constant_value -> string

val cast_value: idltype -> constant_value -> constant_value

val eval_int: lexpr -> int
val eval: lexpr -> constant_value

val tostring: Prefix.t -> lexpr -> string
val output: out_channel -> Prefix.t * lexpr -> unit

val is_free: string -> lexpr -> bool
val is_dependent: string -> idltype -> bool

(* Expand a typedef name, returning its definition *)
val expand_typedef: (string -> idltype) ref


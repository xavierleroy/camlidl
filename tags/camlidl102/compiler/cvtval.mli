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

(* $Id: cvtval.mli,v 1.10 2000-08-19 11:04:56 xleroy Exp $ *)

(* Conversion of values between ML and C *)

open Idltypes

val ml_to_c :
  out_channel -> bool -> string -> idltype -> string -> string -> unit
val c_to_ml :
  out_channel -> string -> idltype -> string -> string -> unit
val allocate_output_space :
  out_channel -> string -> idltype -> unit

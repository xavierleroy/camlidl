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

(* $Id: cvtval.mli,v 1.8 1999-02-19 14:33:28 xleroy Exp $ *)

(* Conversion of values between ML and C *)

open Idltypes

val ml_to_c :
  out_channel -> bool -> string -> idltype -> string -> string -> unit
val c_to_ml :
  out_channel -> string -> idltype -> string -> string -> unit

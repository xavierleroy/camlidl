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

(* $Id: cvtval.mli,v 1.9 2000-08-18 11:23:03 xleroy Exp $ *)

(* Conversion of values between ML and C *)

open Idltypes

val ml_to_c :
  out_channel -> bool -> string -> idltype -> string -> string -> unit
val c_to_ml :
  out_channel -> string -> idltype -> string -> string -> unit
val allocate_output_space :
  out_channel -> string -> idltype -> unit

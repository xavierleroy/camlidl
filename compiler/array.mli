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

(* $Id: array.mli,v 1.3 1999-02-19 14:33:26 xleroy Exp $ *)

(* Marshaling for arrays *)

open Idltypes

val array_ml_to_c : 
  (out_channel -> bool -> string -> idltype -> string -> string -> unit) ->
    out_channel -> bool -> string -> array_attributes -> idltype -> string -> string ->
      unit
val array_c_to_ml : 
  (out_channel -> string -> idltype -> string -> string -> unit) ->
    out_channel -> string -> array_attributes -> idltype -> string -> string ->
      unit


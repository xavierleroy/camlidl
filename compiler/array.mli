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

(* $Id: array.mli,v 1.4 2000-08-11 13:26:02 xleroy Exp $ *)

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
val size_out_param : string -> array_attributes -> string

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

(* $Id: array.mli,v 1.5 2000-08-18 11:23:02 xleroy Exp $ *)

(* Marshaling for arrays and bigarrays *)

open Idltypes

val array_ml_to_c : 
  (out_channel -> bool -> string -> idltype -> string -> string -> unit) ->
    out_channel -> bool -> string -> array_attributes -> idltype -> string -> string ->
      unit
val array_c_to_ml : 
  (out_channel -> string -> idltype -> string -> string -> unit) ->
    out_channel -> string -> array_attributes -> idltype -> string -> string ->
      unit
val array_allocate_output_space :
  out_channel -> array_attributes -> idltype -> string -> unit

val bigarray_ml_to_c :
  out_channel -> string -> bigarray_attributes -> idltype ->
    string -> string -> unit
val bigarray_c_to_ml :
  out_channel -> string -> bigarray_attributes -> idltype ->
    string -> string -> unit
val bigarray_allocate_output_space :
  out_channel -> bigarray_attributes -> idltype -> string -> unit


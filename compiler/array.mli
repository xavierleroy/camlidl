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

(* $Id: array.mli,v 1.6 2000-08-19 11:04:56 xleroy Exp $ *)

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


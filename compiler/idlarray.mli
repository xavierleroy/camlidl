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

(* $Id: array.mli,v 1.7 2002-01-16 09:42:00 xleroy Exp $ *)

(* Marshaling for arrays and bigarrays *)

open Idltypes

val array_ml_to_c : 
  (out_channel -> bool -> Prefix.t -> idltype -> string -> string -> unit) ->
    out_channel -> bool -> Prefix.t -> array_attributes -> idltype -> string -> string ->
      unit
val array_c_to_ml : 
  (out_channel -> Prefix.t -> idltype -> string -> string -> unit) ->
    out_channel -> Prefix.t -> array_attributes -> idltype -> string -> string ->
      unit
val array_allocate_output_space :
  out_channel -> Prefix.t -> array_attributes -> idltype -> string -> unit

val bigarray_ml_to_c :
  out_channel -> Prefix.t -> bigarray_attributes -> idltype ->
    string -> string -> unit
val bigarray_c_to_ml :
  out_channel -> Prefix.t -> bigarray_attributes -> idltype ->
    string -> string -> unit
val bigarray_allocate_output_space :
  out_channel -> Prefix.t -> bigarray_attributes -> idltype -> string -> unit


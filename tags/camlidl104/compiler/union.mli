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

(* $Id: union.mli,v 1.6 2002-01-16 09:42:04 xleroy Exp $ *)

(* Marshalling for unions *)

open Idltypes

val union_ml_to_c : 
  (out_channel -> bool -> Prefix.t -> idltype -> string -> string -> unit) ->
    out_channel -> bool -> Prefix.t -> union_decl -> string -> string -> string -> unit
val union_c_to_ml : 
  (out_channel -> Prefix.t -> idltype -> string -> string -> unit) ->
    out_channel -> Prefix.t -> union_decl -> string -> string -> string -> unit

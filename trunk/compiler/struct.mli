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

(* $Id: struct.mli,v 1.7 2002-01-16 09:42:03 xleroy Exp $ *)

(* Marshaling for structs *)

open Idltypes

val struct_ml_to_c : 
  (out_channel -> bool -> Prefix.t -> idltype -> string -> string -> unit) ->
    out_channel -> bool -> Prefix.t -> struct_decl -> string -> string -> unit
val struct_c_to_ml : 
  (out_channel -> Prefix.t -> idltype -> string -> string -> unit) ->
    out_channel -> Prefix.t -> struct_decl -> string -> string -> unit

val remove_dependent_fields: field list -> field list

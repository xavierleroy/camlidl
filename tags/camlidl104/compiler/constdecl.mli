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

(* $Id: constdecl.mli,v 1.8 2000-08-19 11:04:56 xleroy Exp $ *)

(* Handling of constant declarations *)

open Idltypes

type constant_decl =
  { cd_name: string; cd_type: idltype; cd_value: lexpr }

val ml_declaration: out_channel -> constant_decl -> unit
val c_declaration: out_channel -> constant_decl -> unit
val ml_definition: out_channel -> constant_decl -> unit
val record: constant_decl -> unit

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

(* $Id: constdecl.mli,v 1.7 1999-02-19 14:33:27 xleroy Exp $ *)

(* Handling of constant declarations *)

open Idltypes

type constant_decl =
  { cd_name: string; cd_type: idltype; cd_value: lexpr }

val ml_declaration: out_channel -> constant_decl -> unit
val c_declaration: out_channel -> constant_decl -> unit
val ml_definition: out_channel -> constant_decl -> unit
val record: constant_decl -> unit

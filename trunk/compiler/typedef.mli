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

(* $Id: typedef.mli,v 1.8 2000-08-19 11:04:58 xleroy Exp $ *)

(* Handling of typedefs *)

open Idltypes

type type_decl =
  { td_name: string;
    td_mod: string;
    td_type: idltype;
    td_abstract: bool;
    td_c2ml: string option;
    td_ml2c: string option;
    td_errorcode: bool;
    td_errorcheck: string option;
    td_mltype: string option }

val ml_declaration: out_channel -> type_decl -> unit
val c_declaration: out_channel -> type_decl -> unit
val emit_transl: out_channel -> type_decl -> unit
val declare_transl: out_channel -> type_decl -> unit

val find: (string -> type_decl) ref

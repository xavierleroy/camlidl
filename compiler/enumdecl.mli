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

(* $Id: enumdecl.mli,v 1.5 2000-08-19 11:04:56 xleroy Exp $ *)

(* Generation of converters for enums *)

open Idltypes

val ml_declaration : out_channel -> enum_decl -> unit
val c_declaration : out_channel -> enum_decl -> unit
val declare_transl: out_channel -> enum_decl -> unit
val emit_transl : out_channel -> enum_decl -> unit
val emit_transl_table : out_channel -> enum_decl -> unit

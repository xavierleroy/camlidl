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

(* $Id: enumdecl.mli,v 1.3 1999-02-19 14:33:29 xleroy Exp $ *)

(* Generation of converters for enums *)

open Idltypes

val ml_declaration : out_channel -> enum_decl -> unit
val c_declaration : out_channel -> enum_decl -> unit
val declare_transl: out_channel -> enum_decl -> unit
val emit_transl : out_channel -> enum_decl -> unit

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

(* $Id: structdecl.mli,v 1.4 1999-03-09 16:27:02 xleroy Exp $ *)

(* Generation of converters for structs *)

open Idltypes

val ml_declaration : out_channel -> struct_decl -> unit
val c_declaration : out_channel -> struct_decl -> unit
val declare_transl: out_channel -> struct_decl -> unit
val emit_transl : out_channel -> struct_decl -> unit

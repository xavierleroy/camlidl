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

(* $Id: predef.mli,v 1.2 1999-02-19 14:33:38 xleroy Exp $ *)

(* Predefined types and interfaces *)

val typedefs: Typedef.type_decl list
val interfaces: Intf.interface list

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

(* $Id: fixlabels.mli,v 1.1 1999-03-16 15:40:52 xleroy Exp $ *)

(* Prefix record labels with struct/typedef name *)

val prefix_file: File.components -> File.components

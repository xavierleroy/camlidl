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

(* $Id: lexer_midl.mli,v 1.1 1999-02-22 10:00:15 xleroy Exp $ *)

val token: Lexing.lexbuf -> Parser_midl.token

exception Lex_error of string

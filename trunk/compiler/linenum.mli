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

(* $Id: linenum.mli,v 1.4 2001-06-29 13:30:00 xleroy Exp $ *)

(* An auxiliary lexer for determining the line number corresponding to
   a file position, honoring the directives # linenum "filename" *)

val for_position: string -> int -> string * int * int
        (* [Linenum.for_position file loc] returns a triple describing
           the location [loc] in the file named [file].
           First result is name of actual source file.
           Second result is line number in that source file.
           Third result is position of beginning of that line in [file]. *)

val print_location: out_channel -> unit
        (* Print the current location as determined by [for_position]. *)

val current_file: string ref
val current_lexbuf: Lexing.lexbuf ref
        (* Name and lexbuf on file currently read *)

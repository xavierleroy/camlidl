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

(* $Id: prefix.mli,v 1.1 2002-01-16 09:42:03 xleroy Exp $ *)

open Idltypes

type t

val empty: t
val enter_function: (string * 'a * 'b) list -> t
val enter_struct: t -> struct_decl -> string -> t
val for_ident: t -> string -> string

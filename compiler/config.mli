(***********************************************************************)
(*                                                                     *)
(*                              CamlIDL                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Lesser General Public License LGPL v2.1 *)
(*                                                                     *)
(***********************************************************************)

(* $Id: config.mli,v 1.4 2001-06-29 13:29:59 xleroy Exp $ *)

(* Compile-time configuration *)

(* How to invoke the C preprocessor *)
val cpp: string

(* The C names for 64-bit signed and unsigned integers *)
val int64_type: string
val uint64_type: string

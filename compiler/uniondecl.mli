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

(* $Id: uniondecl.mli,v 1.5 2000-08-19 11:04:58 xleroy Exp $ *)

(* Generation of converters for unions *)

open Idltypes

val ml_declaration : out_channel -> union_decl -> unit
val c_declaration : out_channel -> union_decl -> unit
val declare_transl: out_channel -> union_decl -> unit
val emit_transl : out_channel -> union_decl -> unit

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

(* $Id: typedef.mli,v 1.9 2002-04-19 13:24:30 xleroy Exp $ *)

(* Handling of typedefs *)

open Idltypes

val ml_declaration: out_channel -> type_decl -> unit
val c_declaration: out_channel -> type_decl -> unit
val emit_transl: out_channel -> type_decl -> unit
val declare_transl: out_channel -> type_decl -> unit

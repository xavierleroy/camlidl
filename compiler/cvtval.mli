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

(* $Id: cvtval.mli,v 1.11 2002-01-16 09:42:01 xleroy Exp $ *)

(* Conversion of values between ML and C *)

open Idltypes

val ml_to_c :
  out_channel -> bool -> Prefix.t -> idltype -> string -> string -> unit
val c_to_ml :
  out_channel -> Prefix.t -> idltype -> string -> string -> unit
val allocate_output_space :
  out_channel -> Prefix.t -> string -> idltype -> unit

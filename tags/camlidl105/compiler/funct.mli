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

(* $Id: funct.mli,v 1.11 2004-07-08 09:50:23 xleroy Exp $ *)

(* Generation of stub code for functions *)

open Idltypes

type in_out =
    In | Out | InOut

type function_decl =
  { fun_name: string;
    fun_mod: string;
    fun_res: idltype;
    fun_params: (string * in_out * idltype) list;
    fun_mlname: string option;
    fun_call: string option;
    fun_dealloc: string option;
    fun_blocking: bool }

val ml_view :
      function_decl -> (string * idltype) list * (string * idltype) list

val ml_declaration : out_channel -> function_decl -> unit
val c_declaration : out_channel -> function_decl -> unit

val emit_wrapper : out_channel -> function_decl -> unit
val emit_method_wrapper : out_channel -> string -> function_decl -> unit

val out_inout : out_channel -> in_out -> unit

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

(* $Id: intf.mli,v 1.8 1999-02-19 14:33:31 xleroy Exp $ *)

(* Handling of COM-style interfaces *)

open Idltypes
open Funct

type interface =
  { intf_name: string;                  (* Name of interface *)
    intf_mod: string;                   (* Source module *)
    mutable intf_super: interface;      (* Super-interface *)
    mutable intf_methods: function_decl list;   (* Methods *)
    mutable intf_uid: string }          (* Unique interface ID *)

val ml_declaration: out_channel -> interface -> unit
val ml_class_declaration: out_channel -> interface -> unit
val c_declaration: out_channel -> interface -> unit
val ml_class_definition: out_channel -> interface -> unit
val emit_transl: out_channel -> interface -> unit
val declare_transl: out_channel -> interface -> unit

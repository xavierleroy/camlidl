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

(* $Id: cvttyp.mli,v 1.8 1999-02-19 14:33:28 xleroy Exp $ *)

open Idltypes

(* Convert an IDL type to a C declarator *)
val out_c_decl : out_channel -> string * Idltypes.idltype -> unit

(* Convert an IDL type to a C type *)
val out_c_type : out_channel -> Idltypes.idltype -> unit

(* Print C declarations for structs, unions, enums *)
val out_struct : out_channel -> struct_decl -> unit
val out_union : out_channel -> union_decl -> unit
val out_enum : out_channel -> enum_decl -> unit

(* Convert an IDL type to an ML type *)
val out_ml_type : out_channel -> Idltypes.idltype -> unit

(* Convert a list of IDL types to an ML type *)
val out_ml_types :
      out_channel -> string -> ('a * Idltypes.idltype) list -> unit

(* Print an ML type name, qualified if necessary *)
val out_mltype_name: out_channel -> string * string -> unit

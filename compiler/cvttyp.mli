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

(* $Id: cvttyp.mli,v 1.12 2002-01-16 16:15:31 xleroy Exp $ *)

open Idltypes

(* Convert an IDL type to a C declarator *)
val out_c_decl : out_channel -> string * idltype -> unit

(* Convert an IDL type to a C type *)
val out_c_type : out_channel -> idltype -> unit

(* Print C declarations for structs, unions, enums *)
val out_struct : out_channel -> struct_decl -> unit
val out_union : out_channel -> union_decl -> unit
val out_enum : out_channel -> enum_decl -> unit

(* Convert an IDL type to an ML type *)
val out_ml_type: out_channel -> idltype -> unit

(* Convert a list of IDL types to an ML type *)
val out_ml_types: out_channel -> string -> ('a * idltype) list -> unit

(* Print an ML type name, qualified if necessary *)
val out_mltype_name: out_channel -> string * string -> unit

(* Expand typedef and const in type *)
val scrape_type: idltype -> idltype

(* Remove leading "const" from a type *)
val scrape_const: idltype -> idltype

(* Determine if a type is an ignored pointer *)
val is_ignored: idltype -> bool

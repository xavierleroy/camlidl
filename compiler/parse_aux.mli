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

(* $Id: parse_aux.mli,v 1.10 2002-01-16 16:15:33 xleroy Exp $ *)

(* Auxiliary functions for parsing *)

open Idltypes
open Funct
open Typedef
open File

module StringSet : Set.S with type elt = string

val null_attr_var : lexpr
    (* Represents missing attr var in attributes such as size_is(,f) *)
val no_bounds : array_attributes
    (* Array type without bounds *)
val one_bound : lexpr -> array_attributes
    (* Array type with upper bound *)
val no_switch : union_attributes
    (* Represents an unknown switch for an union *)
val no_enum_attr : enum_attributes
    (* Default attributes for enums *)
val pointer_default : pointer_kind ref
    (* Default pointer kind *)
val int_default : integer_repr ref
val long_default : integer_repr ref
    (* Default integer representation for "int" and "long" types *)
val make_param :
  (string * lexpr list) list -> idltype -> (idltype -> string * idltype) ->
        string * in_out * idltype
    (* Build a function parameter *)
val make_fun_declaration :
  (string * lexpr list) list ->
  idltype ->
  string ->
  (string * in_out * idltype) list ->
  (string * string) list -> function_decl
    (* Build a function declaration *)
val make_field :
  (string * lexpr list) list ->
  idltype -> (idltype -> string * idltype) -> field
    (* Build a field declaration *)
val make_fields :
  (string * lexpr list) list ->
  idltype -> (idltype -> string * idltype) list -> field list
    (* Build a list of field declarations *)
val make_discriminated_union :
  string ->
  string ->
  string ->
  idltype -> union_case list -> struct_decl
    (* Convert a union switch(...) into a struct encapsulating an union *)
val type_names : StringSet.t ref
    (* The set of type names (defined by typedef or interface) seen so far *)
val make_typedef :
  (string * lexpr list) list ->
  idltype ->
  (idltype -> string * idltype) list ->
  type_decl list
    (* Build a typedef declaration *)
val make_const_decl :
  (string * lexpr list) list ->
  idltype ->
  string ->
  lexpr ->
  Constdecl.constant_decl
    (* Build a constant declaration *)
val update_defaults : (string * lexpr list) list -> unit
    (* Update [!pointer_default], [!int_default] and [!long_default]
       according to the given attr list *)
val save_defaults : unit -> unit
val restore_defaults : unit -> unit
    (* Save or restore the current defaults on a stack *)
val make_interface :
  string -> (string * lexpr list) list -> string option -> components ->
    components
    (* Build an interface declaration *)
val make_forward_interface : string -> component
    (* Build a forward declaration for an interface *)
val make_diversion : string * string -> diversion_type * string
    (* Represent a diversion *)
val make_int : integer_kind -> idltype
    (* Build an integer type (without [signed] or [unsigned] modifier) *)
val make_unsigned : integer_kind -> idltype
    (* Build an integer type (with explicit [unsigned] modifier) *)
val make_signed : integer_kind -> idltype
    (* Build an integer type (with explicit [signed] modifier) *)
val handle_t_type : unit -> idltype
val wchar_t_type : unit -> idltype
    (* Warn about unsupported types [handle_t] and [wchar_t] *)
val make_star_attribute : string * 'a -> string * 'a
    (* Apply a [*] modifier to an attribute *)
val make_type_const : idltype -> idltype
    (* Add a "const" modifier to a type *)
val read_file : (string -> components) ref
    (* Forward declaration of [Parse.read_file] *)
val read_import : string -> components
    (* Read an import file *)

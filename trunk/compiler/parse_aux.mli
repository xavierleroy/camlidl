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

(* $Id: parse_aux.mli,v 1.3 1999-02-19 14:33:36 xleroy Exp $ *)

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
val make_param :
  (string * lexpr list) list -> idltype -> (idltype -> string * idltype) ->
        string * in_out * idltype
    (* Build a function parameter *)
val make_fun_declaration :
  (string * lexpr list) list ->
  idltype ->
  string ->
  (string * in_out * idltype) list ->
  string option -> function_decl
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
val update_pointer_default : (string * lexpr list) list -> unit
    (* Update [!pointer_default] according to the given attr list *)
val make_interface :
  string -> (string * lexpr list) list -> string option -> components ->
    components
    (* Build an interface declaration *)
val make_forward_interface : string -> component
    (* Build a forward declaration for an interface *)
val make_diversion : string * string -> diversion_type * string
    (* Represent a diversion *)
val make_unsigned : integer_kind -> idltype
    (* Apply the [unsigned] modifier to an integer type *)
val make_signed : integer_kind -> idltype
    (* Apply the [signed] modifier to an integer type *)
val handle_t_type : unit -> idltype
val hyper_type : unit -> integer_kind
val wchar_t_type : unit -> idltype
    (* Warn about unsupported types [handle_t] [hyper] [wchar_t] *)
val make_star_attribute : string * 'a -> string * 'a
    (* Apply a [*] modifier to an attribute *)
val read_file : (string -> components) ref
    (* Forward declaration of [Parse.read_file] *)
val read_import : string -> components
    (* Read an import file *)

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

(* $Id: idltypes.mli,v 1.21 2001-06-29 13:29:59 xleroy Exp $ *)

type integer_kind =
    Int | Long | Hyper | Small | Short | Char
  | UInt | ULong | UHyper | USmall | UShort | UChar
  | SChar | Byte | Boolean

type integer_repr =
    Iunboxed | Inative | I32 | I64

type pointer_kind = Ref | Unique | Ptr | Ignore

type idltype =
    Type_int of integer_kind * integer_repr
  | Type_float
  | Type_double
  | Type_void
  | Type_pointer of pointer_kind * idltype
  | Type_array of array_attributes * idltype
  | Type_bigarray of bigarray_attributes * idltype
  | Type_struct of struct_decl
  | Type_union of union_decl * union_attributes
  | Type_enum of enum_decl * enum_attributes
  | Type_named of string * string (* module name, type name *)
  | Type_interface of string * string (* module name, interface name *)
  | Type_const of idltype

and array_attributes =
  { bound: lexpr option;
    size: lexpr option;
    length: lexpr option;
    is_string: bool;
    maybe_null: bool;
    null_terminated: bool }

and bigarray_attributes =
  { dims: array_attributes list;
    fortran_layout: bool;
    mutable malloced: bool }

and union_attributes =
  { discriminant: lexpr }

and enum_attributes =
  { bitset: bool }

and field =
  { field_name: string; field_mlname: string; field_typ: idltype }

and union_case =
  { case_labels: string list; case_field: field option }

and enum_const =
  { const_name: string; const_val: lexpr option }

and struct_decl =
  { sd_name: string; sd_mod: string; mutable sd_stamp: int;
     mutable sd_fields: field list }

and union_decl =
  { ud_name: string; ud_mod: string; mutable ud_stamp: int;
    mutable ud_cases: union_case list }

and enum_decl =
  { en_name: string; en_mod: string; mutable en_stamp: int;
    mutable en_consts: enum_const list }

and lexpr =
    Expr_ident of string
  | Expr_int of int
  | Expr_string of string
  | Expr_cond of lexpr * lexpr * lexpr
  | Expr_sequand of lexpr * lexpr
  | Expr_sequor of lexpr * lexpr
  | Expr_logor of lexpr * lexpr
  | Expr_logxor of lexpr * lexpr
  | Expr_logand of lexpr * lexpr
  | Expr_eq of lexpr * lexpr
  | Expr_ne of lexpr * lexpr
  | Expr_lt of lexpr * lexpr
  | Expr_gt of lexpr * lexpr
  | Expr_le of lexpr * lexpr
  | Expr_ge of lexpr * lexpr
  | Expr_lshift of lexpr * lexpr
  | Expr_rshift of lexpr * lexpr
  | Expr_plus of lexpr * lexpr
  | Expr_minus of lexpr * lexpr
  | Expr_times of lexpr * lexpr
  | Expr_div of lexpr * lexpr
  | Expr_mod of lexpr * lexpr
  | Expr_neg of lexpr
  | Expr_lognot of lexpr
  | Expr_boolnot of lexpr
  | Expr_deref of lexpr
  | Expr_addressof of lexpr
  | Expr_cast of idltype * lexpr
  | Expr_sizeof of idltype
  | Expr_subscript of lexpr * lexpr
  | Expr_dereffield of lexpr * string
  | Expr_field of lexpr * string


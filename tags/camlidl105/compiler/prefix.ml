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

(* $Id: prefix.ml,v 1.1 2002-01-16 09:42:03 xleroy Exp $ *)

open Idltypes
open Utils

module StringMap = Map.Make(struct type t = string let compare = compare end)

type t = string StringMap.t

let empty = StringMap.empty

let enter_function params =
  List.fold_left (fun e (name, _, _) -> StringMap.add name "" e)
                 StringMap.empty params

let enter_struct pref sd base =
  let base' = base ^ "." in
  List.fold_left
    (fun e f -> StringMap.add f.field_name base' e)
    StringMap.empty sd.sd_fields

let for_ident pref id =
  try
    StringMap.find id pref
  with Not_found ->
    error (Printf.sprintf "Illegal reference to dependent variable %s.  This variable is not in scope." id)

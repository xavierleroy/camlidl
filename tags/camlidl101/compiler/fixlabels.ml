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

(* $Id: fixlabels.ml,v 1.3 2000-08-19 11:04:56 xleroy Exp $ *)

(* Prefix record labels with struct/typedef name if required or requested *)

open Printf
open Utils
open Idltypes
open Typedef
open Funct
open Intf
open File

(* Determine if an mlname was provided by the user in the IDL file *)

let no_ml_name f = f.field_mlname == f.field_name
    (* We use physical equality instead of string equality
       so that an explicit [mlname(samename)] can override the prefixing *)

(* Collect all label names and those that appear at least twice *)

module LabelSet = Set.Make(struct type t = string let compare = compare end)

let all_labels = ref LabelSet.empty
let repeated_labels = ref LabelSet.empty

let add_label s =
  if LabelSet.mem s !all_labels then
    repeated_labels := LabelSet.add s !repeated_labels
  else
    all_labels := LabelSet.add s !all_labels

let rec collect_type = function
    Type_pointer(_, ty) -> collect_type ty
  | Type_array(_, ty) -> collect_type ty
  | Type_bigarray(_, ty) -> collect_type ty
  | Type_struct sd -> List.iter collect_field sd.sd_fields
  | Type_union(ud, _) -> List.iter collect_case ud.ud_cases
  | _ -> ()

and collect_field f =
  if no_ml_name f then add_label f.field_name;
  collect_type f.field_typ

and collect_case c =
  match c.case_field with None -> () | Some f -> collect_field f

let collect_component = function
    Comp_typedecl td -> collect_type td.td_type
  | Comp_structdecl sd -> List.iter collect_field sd.sd_fields
  | Comp_uniondecl ud -> List.iter collect_case ud.ud_cases
  | Comp_fundecl fd -> collect_type fd.fun_res
  | Comp_interface intf ->
      List.iter (fun fd -> collect_type fd.fun_res) intf.intf_methods
  | _ -> ()

(* A struct definition needs prefixing if some of its labels occur
   several times in the file *)

let need_prefixing sd =
  List.exists
    (fun f -> no_ml_name f && LabelSet.mem f.field_name !repeated_labels)
    sd.sd_fields

(* Prefix label names with struct or typedef name, if required or requested *)

let choose_prefix oldpref newpref =
  if newpref <> "" then newpref else oldpref

let rec prefix_type pref = function
    Type_struct sd -> Type_struct(prefix_struct pref sd)
  | Type_union(ud, attr) -> Type_union(prefix_union pref ud, attr)
  | Type_pointer(kind, ty) -> Type_pointer(kind, prefix_type pref ty)
  | Type_array(attr, ty) -> Type_array(attr, prefix_type pref ty)
  | ty -> ty

and prefix_struct pref sd =
  let prefix = choose_prefix pref sd.sd_name in
  let add_prefix =
    if !Clflags.prefix_all_labels || need_prefixing sd then begin
      if prefix = "" then begin
        eprintf "Warning: couldn't find prefix for anonymous struct\n";
        false
      end else
        true
    end else
      false in
  {sd with sd_fields = List.map (prefix_field add_prefix prefix) sd.sd_fields}

and prefix_field add_prefix pref f =
  let new_mlname =
    if add_prefix && no_ml_name f
    then pref ^ "_" ^ f.field_name
    else f.field_mlname in
  {f with field_mlname = new_mlname;
          field_typ = prefix_type pref f.field_typ}

and prefix_union pref ud =
  let prefix = choose_prefix pref ud.ud_name in
  {ud with ud_cases = List.map (prefix_case prefix) ud.ud_cases}
  
and prefix_case pref cs =
    match cs.case_field with
      None -> cs
    | Some ty -> {cs with case_field = Some(prefix_field false pref ty)}

let prefix_typedecl td =
  {td with td_type = prefix_type td.td_name td.td_type}

let prefix_fundecl fd =
  {fd with fun_res = prefix_type "" fd.fun_res}
  (* no struct decl in function arguments *)

let prefix_interface intf =
  {intf with intf_methods = List.map prefix_fundecl intf.intf_methods}

let prefix_component = function
    Comp_typedecl td -> Comp_typedecl(prefix_typedecl td)
  | Comp_structdecl sd -> Comp_structdecl(prefix_struct "" sd)
  | Comp_uniondecl ud -> Comp_uniondecl(prefix_union "" ud)
  | Comp_fundecl fd -> Comp_fundecl(prefix_fundecl fd)
  | Comp_interface intf -> Comp_interface(prefix_interface intf)
  | cmp -> cmp

let prefix_file f =
  if !Clflags.keep_labels then f else begin
    all_labels := LabelSet.empty;
    repeated_labels := LabelSet.empty;
    List.iter collect_component f;
    let res = List.map prefix_component f in
    all_labels := LabelSet.empty;
    repeated_labels := LabelSet.empty;
    res
  end



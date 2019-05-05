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

(* $Id: predef.ml,v 1.6 2002-04-19 13:24:29 xleroy Exp $ *)

(* Predefined types and interfaces *)

open Idltypes
open Typedef
open Intf

let hresult =
  { td_name = "HRESULT"; td_mod = "Com";
    td_type = Type_int(Long, Iunboxed);
    td_abstract = false;
    td_c2ml = None; td_ml2c = None;
    td_finalize = None; td_compare = None; td_hash = None;
    td_errorcode = true;
    td_errorcheck = Some "camlidl_check_hresult";
    td_mltype = None }

let hresult_bool =
  { td_name = "HRESULT_bool"; td_mod = "Com";
    td_type = Type_int(Long, Iunboxed);
    td_abstract = false;
    td_c2ml = None; td_ml2c = None;
    td_finalize = None; td_compare = None; td_hash = None;
    td_errorcode = false;
    td_errorcheck = Some "camlidl_check_hresult";
    td_mltype = Some "bool" }

let hresult_int =
  { td_name = "HRESULT_int"; td_mod = "Com";
    td_type = Type_int(Long, Iunboxed);
    td_abstract = false;
    td_c2ml = None; td_ml2c = None;
    td_finalize = None; td_compare = None; td_hash = None;
    td_errorcode = false;
    td_errorcheck = Some "camlidl_check_hresult";
    td_mltype = Some "int" }

let bstr =
  { td_name = "BSTR"; td_mod = "Com";
    td_type = Type_int(Long, Iunboxed);
    td_abstract = false;
    td_c2ml = None; td_ml2c = None;
    td_finalize = None; td_compare = None; td_hash = None;
    td_errorcode = false;
    td_errorcheck = None;
    td_mltype = Some "string" }

let rec iunknown =
  { intf_name = "IUnknown"; intf_mod = "Com";
    intf_super = iunknown;
    intf_methods = [];
    intf_uid = "00000000-0000-0000-C000-000000000046" }

let idispatch =
  { intf_name = "IDispatch"; intf_mod = "Com";
    intf_super = iunknown;
    intf_methods = [];
    intf_uid = "00020400-0000-0000-C000-000000000046" }

let typedefs = [hresult; hresult_bool; hresult_int; bstr]
let interfaces = [iunknown; idispatch]

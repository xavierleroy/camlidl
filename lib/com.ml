(***********************************************************************)
(*                                                                     *)
(*                              CamlIDL                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License.         *)
(*                                                                     *)
(***********************************************************************)

(* $Id: com.ml,v 1.8 2000-08-19 11:04:59 xleroy Exp $ *)

(* Run-time library for COM components *)

type 'a interface

type 'a iid

type 'a opaque

type clsid = string

exception Error of int * string * string

external initialize : unit -> unit = "camlidl_com_initialize"
external uninitialize : unit -> unit = "camlidl_com_uninitialize"

external query_interface: 'a interface -> 'b iid -> 'b interface =
  "camlidl_com_queryInterface"

type iUnknown
type iDispatch

let iUnknown_of (intf : 'a interface) = (Obj.magic intf : iUnknown interface)

let _ =
  Callback.register "Oo.new_method" Oo.new_method;
  Callback.register_exception "Com.Error" (Error(0, "", ""))

external combine: 'a interface -> 'b interface -> 'a interface =
  "camlidl_com_combine"

external clsid: string -> clsid = "camlidl_com_parse_uid"
external _parse_iid: string -> 'a iid = "camlidl_com_parse_uid"

external create_instance : clsid -> 'a iid -> 'a interface
    = "camlidl_com_create_instance"
  
type 'a component_factory =
  { create : unit -> 'a interface;
    clsid : clsid;
    friendly_name : string;
    ver_ind_prog_id : string;
    prog_id : string }

external register_factory : 'a component_factory -> unit
    = "camlidl_com_register_factory"

type hRESULT_int = int
type hRESULT_bool = bool
type bSTR = string

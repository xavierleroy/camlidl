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

(* $Id: com.mli,v 1.6 1999-02-19 14:33:45 xleroy Exp $ *)

(* Run-time library for COM components *)

type 'a interface

type 'a iid

type 'a opaque

type clsid

exception Error of int * string * string

val initialize : unit -> unit
val uninitialize : unit -> unit

val query_interface : 'a interface -> 'b iid -> 'b interface

type iUnknown

val iUnknown_of : 'a interface -> iUnknown interface

val combine : 'a interface -> 'b interface -> 'a interface

val clsid : string -> clsid

val create_instance : clsid -> 'a iid -> 'a interface

type 'a component_factory =
  { create : unit -> 'a interface;
    clsid : clsid;
    friendly_name : string;
    ver_ind_prog_id : string;
    prog_id : string }

val register_factory : 'a component_factory -> unit

(*---*)

val _parse_iid : string -> 'a iid

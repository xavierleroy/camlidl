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

(* $Id: com.mli,v 1.9 1999-03-16 15:41:07 xleroy Exp $ *)

(* Module [Com]: run-time library for COM components *)

type 'a interface
        (* The type of COM components implementing interface ['a] *)
type 'a iid
        (* The type of the interface identifier for interface ['a] *)
type clsid
        (* The type of component identifiers *)
type 'a opaque
        (* The type representing opaque pointers to values of type ['a].
           Opaque pointers are pointers with attribute [ptr] in IDL files. *)

exception Error of int * string * string
        (* Exception raised to report Com errors.
           The arguments are [Error(errcode, who, what)].
           [errcode] is the Com error code ([HRESULT] code)
           with the high bit clear.
           [who] identifies the function or method that raised the exception.
           [what] is a message explaining the cause of the error. *)

val initialize : unit -> unit
        (* Initialize the COM library.  Must be called once before
           using any function in this module.  [Com.initialize]
           can be called several times, provided that [Com.uninitialize]
           is called an equal number of times before the program exits. *)
val uninitialize : unit -> unit
        (* Terminate the COM library. *)

val query_interface : 'a interface -> 'b iid -> 'b interface
        (* [Com.query_interface comp iid] asks the component [comp]
           whether it supports the interface identified by [iid].
           If yes, it returns the corresponding interface of the component.
           If not, it raises [Com.Error]. *)

type iUnknown
        (* The type of the interface [IUnknown], from which all other
           interfaces derive. *)

type iDispatch
        (* The type of the interface [IDispatch], from which all
           dispatch interfaces derive. *)

val iUnknown_of : 'a interface -> iUnknown interface
        (* Return the [IUnknown] interface of the given component.
           This operation never fails, since all components support
           the [IUnknown] interface. *)

val combine : 'a interface -> 'b interface -> 'a interface
        (* Combine the interfaces of two components.
           [Com.combine c1 c2] returns a component that supports the
           union of the interfaces supported by [c1] and [c2].
           When queried for an interface, the resulting component
           delegates its implementation to [c1] if [c1] implements that
           interface, and otherwise delegates its implementation to [c2]. *)

val clsid : string -> clsid
        (* Parse the string representation of a component identifier
           ([hex8-hex4-hex4-hex4-hex12], where [hexN] represents [N]
            hexadecimal digits). *)

val create_instance : clsid -> 'a iid -> 'a interface
        (* [Com.create_instance clsid iid] creates an instance of
           the component identified by [clsid], and returns its [iid]
           interface.  The implementation of the component is searched
           in the registry; if the component is implemented in a DLL,
           the DLL is loaded in memory if necessary; if the component
           is implemented in a separate server process, the server is
           started if necessary.  Raise [Com.Error] if the component
           [clsid] cannot be found, or if it does not support interface
           [iid]. *)

type 'a component_factory =
  { create : unit -> 'a interface;
    clsid : clsid;
    friendly_name : string;
    ver_ind_prog_id : string;
    prog_id : string }
        (* Informations required for registering a Caml implementation
           of a component.
           [create] is a function that returns a fresh instance
           of the component.
           [clsid] is the component identifier.
           [friendly_name] is a short description of the component
           (for information only).
           [ver_ind_prog_id] and [prog_id] are symbolic names for the
           component.  By convention, [prog_id] is [ver_ind_prog_id] plus
           a version number at the end, i.e. [ver_ind_prog_id] is
           ["MyCamlComponent"] while [prog_id] is ["MyCamlComponent.3"]. *)

val register_factory : 'a component_factory -> unit
        (* Register a Caml implementation of a component.
           [Com.register_factory f] stores the component factory [f]
           in the registry.  Other programs can then create instances
           of the component by calling [CreateInstance] from C and C++
           or [Com.create_instance] from Caml. *)

type hRESULT_int = int
type hRESULT_bool = bool
type bSTR = string
        (* The Caml types corresponding to the IDL types [HRESULT_int],
           [HRESULT_bool] and [BSTR], respectively. *)

(*--*)

val _parse_iid : string -> 'a iid

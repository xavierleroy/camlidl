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

(* $Id: variables.mli,v 1.6 1999-02-19 14:33:43 xleroy Exp $ *)

(* Generate temporaries *)

val new_var : string -> string
val new_c_variable : Idltypes.idltype -> string
val new_ml_variable : unit -> string
val new_ml_variable_block : int -> string
val output_variable_declarations : out_channel -> unit
val init_value_block : out_channel -> string -> int -> unit
val copy_values_to_block : out_channel -> string -> string -> int -> unit
val need_context : bool ref

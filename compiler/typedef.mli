(* Handling of typedefs *)

open Idltypes

type type_decl =
  { td_name: string;
    td_mod: string;
    td_type: idltype;
    td_abstract: bool;
    td_c2ml: string option;
    td_ml2c: string option;
    td_errorcode: bool;
    td_errorcheck: string option;
    td_mltype: string option }

val ml_declaration: out_channel -> type_decl -> unit
val c_declaration: out_channel -> type_decl -> unit
val emit_transl: out_channel -> type_decl -> unit
val declare_transl: out_channel -> type_decl -> unit

val find: (string -> type_decl) ref
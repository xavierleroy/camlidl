(* Handling of interfaces *)

open Idltypes

type diversion_type = Div_c | Div_ml | Div_mli | Div_ml_mli

type component =
    Comp_typedecl of Typedef.type_decl
  | Comp_structdecl of struct_decl
  | Comp_uniondecl of union_decl
  | Comp_enumdecl of enum_decl
  | Comp_fundecl of Funct.function_decl
  | Comp_constdecl of Constdecl.constant_decl
  | Comp_diversion of diversion_type * string
  | Comp_interface of Intf.interface

type idl_file = component list

val gen_mli_file: out_channel -> idl_file -> idl_file -> unit
val gen_ml_file: out_channel -> idl_file -> idl_file -> unit
val gen_c_stub: out_channel -> idl_file -> idl_file -> unit

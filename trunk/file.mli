(* Handling of interfaces *)

open Idltypes

type diversion_type = Div_c | Div_ml

type component =
    Comp_typedecl of Typedef.type_decl
  | Comp_structdecl of struct_decl
  | Comp_uniondecl of union_decl
  | Comp_enumdecl of enum_decl
  | Comp_fundecl of Funct.function_decl
  | Comp_constdecl of Constdecl.constant_decl
  | Comp_diversion of diversion_type * string

type idl_file = component list

val gen_ml_decls: out_channel -> idl_file -> idl_file -> unit
val gen_c_stub: out_channel -> idl_file -> unit

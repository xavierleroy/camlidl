(* Handling of files *)

open Idltypes

type diversion_type = Div_c | Div_h | Div_ml | Div_mli | Div_ml_mli

type component =
    Comp_typedecl of Typedef.type_decl
  | Comp_structdecl of struct_decl
  | Comp_uniondecl of union_decl
  | Comp_enumdecl of enum_decl
  | Comp_fundecl of Funct.function_decl
  | Comp_constdecl of Constdecl.constant_decl
  | Comp_diversion of diversion_type * string
  | Comp_interface of Intf.interface
  | Comp_import of string * components

and components = component list

val eval_constants: components -> unit
val gen_mli_file: out_channel -> components -> unit
val gen_ml_file: out_channel -> components -> unit
val gen_c_stub: out_channel -> components -> unit
val gen_c_header: out_channel -> components -> unit

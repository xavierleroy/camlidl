type restricted_expr =
    Var of string
  | Deref of string

type integer_kind =
    Int | Long | Small | Short | Char
  | UInt | ULong | USmall | UShort | UChar
  | Byte | Boolean

type pointer_kind = Ref | Unique | Ptr | Ignore

type array_attributes =
  { bound: int option;
    size: restricted_expr option;
    length: restricted_expr option;
    is_string: bool;
    null_terminated: bool }

type union_attributes =
  { discriminant: restricted_expr }

type enum_attributes =
  { bitset: bool }

type idltype =
    Type_int of integer_kind
  | Type_float
  | Type_double
  | Type_void
  | Type_pointer of pointer_kind * idltype
  | Type_array of array_attributes * idltype
  | Type_struct of struct_decl
  | Type_union of union_decl * union_attributes
  | Type_enum of enum_decl * enum_attributes
  | Type_named of string

and field =
  { field_name: string; field_typ: idltype }

and union_case =
  { case_label: string option; case_field: field option }

and struct_decl =
  { sd_name: string; mutable sd_stamp: int; mutable sd_fields: field list }

and union_decl =
  { ud_name: string; mutable ud_stamp: int; mutable ud_cases: union_case list }

and enum_decl =
  { en_name: string; mutable en_stamp: int; mutable en_consts: string list }

type in_out =
    In | Out | InOut

type function_decl =
  { fun_name: string;
    fun_res: idltype;
    fun_params: (string * in_out * idltype) list;
    fun_ccall: string option }

type type_decl =
  { td_name: string;
    td_type: idltype;
    td_abstract: bool;
    td_c2ml: string option;
    td_ml2c: string option;
    td_mltype: string option }

type constant_value = Cst_int of int | Cst_string of string

type constant_decl =
  { cd_name: string; cd_type: idltype; cd_value: constant_value }

type interface_component =
    Comp_typedecl of type_decl
  | Comp_structdecl of struct_decl
  | Comp_uniondecl of union_decl
  | Comp_enumdecl of enum_decl
  | Comp_fundecl of function_decl
  | Comp_constdecl of constant_decl
  | Comp_diversion of string

type interface = interface_component list

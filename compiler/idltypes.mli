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
    is_string: bool }

type idltype =
    Type_int of integer_kind
  | Type_float
  | Type_double
  | Type_void
  | Type_pointer of pointer_kind * idltype
  | Type_array of array_attributes * idltype
  | Type_struct of struct_decl
  | Type_union of union_decl * restricted_expr
  | Type_enum of enum_decl
  | Type_named of string

and field = { field_name: string; field_typ: idltype }

and union_case = { case_label: string option; case_field: field option }

and struct_decl = { sd_name: string; sd_stamp: int; sd_fields: field list }

and union_decl = { ud_name: string; ud_stamp: int; ud_cases: union_case list }

and enum_decl = { en_name: string; en_stamp: int; en_consts: string list }

type in_out =
    In | Out | InOut

type function_decl =
  { fun_name: string;
    fun_res: idltype;
    fun_params: (string * in_out * idltype) list }

type type_decl = { td_name: string; td_type: idltype; td_abstract: bool }

type interface_component =
    Comp_typedecl of type_decl
  | Comp_structdecl of struct_decl
  | Comp_uniondecl of union_decl
  | Comp_enumdecl of enum_decl
  | Comp_fundecl of function_decl

type interface = interface_component list

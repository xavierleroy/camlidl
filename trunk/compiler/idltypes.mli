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
  | Type_struct of string
  | Type_union of string * restricted_expr
  | Type_enum of string
  | Type_named of string

type in_out =
    In | Out | InOut

type function_decl =
  { fun_name: string;
    fun_res: idltype;
    fun_params: (string * in_out * idltype) list }

type field = { field_name: string; field_typ: idltype }

type union_case = { case_labels: string list; case_field: field option }

type struct_decl = { sd_name: string; sd_fields: field list }

type union_decl = { ud_name: string; ud_cases: union_case list }

type enum_decl = { en_name: string; en_consts: string list }

type type_decl = { td_name: string; td_type: idltype; td_abstract: bool }

type interface_component =
    Comp_typedecl of type_decl list
  | Comp_structdecl of struct_decl
  | Comp_uniondecl of union_decl
  | Comp_enumdecl of enum_decl
  | Comp_fundecl of function_decl

type interface = interface_component list

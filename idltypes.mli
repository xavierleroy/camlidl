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
  | Type_interface of string

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


file: interface_list EOF
;

interface_list:
    /*empty*/
  | interface_list interface
;

interface:
    /* Valid MIDL attributes: uuid local endpoint version pointer_default
       implicit_handle auto_handle */
    attributes INTERFACE IDENT opt_superinterface
    LBRACE import_list component_list RBRACE
  | attributes INTERFACE IDENT SEMI
;

opt_superinterface:
    /*empty*/
  | COLON IDENT
;

/* Import list */

import_list:
    /* empty */                                 { [] }
  | import_list IMPORT name_list SEMI           { $3 @ $1 }
;

name_list:
    STRING                                      { [$1] }
  | name_list COMMA STRING                      { $3 :: $1 }
;

/* Interface components */

component_list:
    /*empty*/
  | component_list component
;

component:
    CPP_QUOTE LPAREN STRING RPAREN
  | const_decl SEMI
  | type_decl SEMI
  | fn_decl SEMI
;

/* Constant declaration */

const_decl:
    CONST simple_type_spec pointer_opt IDENT EQUAL const_exp
;

const_exp:
    const_int                                   { Cst_int $1 }
  | STRING                                      { Cst_string $1 }
;

const_int:
  | INTEGER                                     { $1 }
  | CHARACTER                                   { Char.code $1 }
  | TRUE                                        { 1 }
  | FALSE                                       { 0 }
  | const_int QUESTIONMARK const_int COLON const_int %prec prec_conditional
                                                { if $1 <> 0 then $3 else $5 }
  | const_int BARBAR const_int                  { if $1 <> 0 then $1 else $3 }
  | const_int AMPERAMPER const_int              { if $1 = 0 then 0 else $3 }
  | const_int BAR const_int                     { $1 land $3 }
  | const_int AMPER const_int                   { $1 lor $3 }
  | const_int CARET const_int                   { $1 lxor $3 }
  | const_int EQUALEQUAL const_int              { if $1 = $3 then 1 else 0 }
  | const_int BANGEQUAL const_int               { if $1 <> $3 then 1 else 0 }
  | const_int LESS const_int                    { if $1 < $3 then 1 else 0 }
  | const_int GREATER const_int                 { if $1 > $3 then 1 else 0 }
  | const_int LESSEQUAL const_int               { if $1 <= $3 then 1 else 0 }
  | const_int GREATEREQUAL const_int            { if $1 >= $3 then 1 else 0 }
  | const_int LESSLESS const_int                { $1 lsl $3 }
  | const_int GREATERGREATER const_int          { $1 asr $3 }
  | const_int PLUS const_int                    { $1 + $3 }
  | const_int MINUS const_int                   { $1 - $3 }
  | const_int STAR const_int                    { $1 * $3 }
  | const_int SLASH const_int               { if $3 = 0 then 0 else $1 / $3 }
  | const_int PERCENT const_int             { if $3 = 0 then 0 else $1 mod $3 }
  | PLUS const_int %prec prec_uminus            { $2 }
  | MINUS const_int %prec prec_uminus           { - $2 }
  | TILDE const_int                             { lnot $2 }
  | BANG const_int                              { if $2 = 0 then 1 else 0 }
  | LPAREN const_int RPAREN                     { $2 }
;

/* Typedef */

type_decl:
    /* Valid MIDL attributes: handle, switch_type, transmit_as,
       ref, unique, ptr, context_handle, ignore, string */
    TYPEDEF attributes type_spec declarators
;

/* Function declaration */

fn_decl:
    /* Valid MIDL attributes: callback, local, ref, unique, ptr, string,
       ignore, context_handle */
    attributes type_spec pointer_opt IDENT
    LPAREN param_list_declarator RPAREN opt_diversion
      { make_op_declaration $1 ($3 $2) $4 $6 $8 }
;
opt_diversion:
    DIVERSION                           { let (id, txt) = $1 in Some txt }
  | /*empty*/                           { None }
;
    
/* Parameter lists */

param_list_declarator:
    VOID                                        { [] }
  | param_declarators                           { List.rev $1 }
;
param_declarators:
    param_declarator                            { [$1] }
  | param_declarators COMMA param_declarator    { $3 :: $1 }
;
param_declarator:
    attributes type_spec declarator
                         { make_param $1 $2 $3 }
;

/* Type specifications */

type_spec:
    simple_type_spec     { $1 }
  | STRUCT IDENT         { Type_struct {sd_name=$2; sd_mod = "";
                                        sd_stamp=0; sd_fields=[]} }
  | struct_declarator    { Type_struct $1 }
  | UNION IDENT          { Type_union({ud_name=$2; ud_mod = "";
                                       ud_stamp=0; ud_cases=[]},
                                      no_switch) }
  | union_declarator     { Type_union($1, no_switch) }
  | ENUM IDENT           { Type_enum({en_name=$2; en_mod = "";
                                      en_stamp=0; en_consts=[]},
                                     no_enum_attr) }
  | enum_declarator      { Type_enum($1, no_enum_attr) }
;

simple_type_spec:
    FLOAT                                       { Type_float }
  | DOUBLE                                      { Type_double }
  | INT                                         { Type_int Int }
  | UNSIGNED INT                                { Type_int UInt }
  | SIGNED INT                                  { Type_int UInt }
  | integer_size opt_int                        { $1 }
  | UNSIGNED integer_size opt_int               { make_unsigned $2 }
  | integer_size UNSIGNED opt_int               { make_unsigned $1 }
  | SIGNED integer_size opt_int                 { $2 }
  | integer_size SIGNED opt_int                 { $1 }
  | CHAR                                        { Type_int Char }
  | UNSIGNED CHAR                               { Type_int UChar }
  | SIGNED CHAR                                 { Type_int Char }
  | BOOLEAN                                     { Type_int Boolean }
  | BYTE                                        { Type_int Byte }
  | VOID                                        { Type_void }
  | IDENT                                       { Type_named("", $1) }
  | HANDLE_T
  | WCHAR_T
  | 
;
integer_size:
    LONG                                        { Type_int Long }
  | SMALL                                       { Type_int Small }
  | SHORT                                       { Type_int Short }
  | HYPER
;
opt_int:
    /* nothing */                               { () }
  | INT                                         { () }
;

/* Struct declaration */

struct_declarator:
    STRUCT opt_ident LBRACE field_declarators RBRACE
                { {sd_name = $2; sd_mod = ""; sd_stamp = 0; sd_fields = $4} } 
;
field_declarators:
    field_declarator                            { $1 }
  | field_declarators field_declarator          { $1 @ $2 }
;
field_declarator:
    /* Valid MIDL attributes: first_is, last_is, length_is, max_is,
       size_is, string, ignore, context_handle, ref, unique, ptr,
       switch_type */
    attributes type_spec declarators SEMI
                        { make_fields $1 $2 (List.rev $3) }
;

/* Union declaration */

union_declarator:
  | UNION opt_ident LBRACE union_body RBRACE
      { {ud_name = $2; ud_mod = ""; ud_stamp = 0; ud_cases = List.rev $4} }
    /* struct with 2 fields: discriminant and union { ... } u */
  | UNION opt_ident SWITCH LPAREN simple_type_spec IDENT RPAREN
    LBRACE union_body RBRACE
;
union_body:
    union_case                                          { $1 }
  | union_body union_case                               { $2 @ $1 }
;
union_case:
    case_list opt_field_declarator SEMI
      { List.map (fun lbl -> {case_label = Some lbl; case_field = $2}) $1 }
  | DEFAULT COLON opt_field_declarator SEMI
      { [{case_label = None; case_field = $3}] }
;
case_list:
    case_label                                          { [$1] }
  | case_list case_label                                { $2 :: $1 }
;
case_label:
    CASE IDENT COLON                                    { $2 }
;
opt_field_declarator:
    /* empty */                                  { None }
  | attributes type_spec declarator              { Some(make_field $1 $2 $3) }
;


/***********************************************************************/
/*                                                                     */
/*                              CamlIDL                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1999 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the Q Public License version 1.0                */
/*                                                                     */
/***********************************************************************/

/* $Id: parser_midl.mly,v 1.19 2002-04-19 14:42:35 xleroy Exp $ */

/* Parser for Microsoft IDL */

%{

open Printf
open Cvttyp
open Idltypes
open Funct
open Typedef
open Constdecl
open Intf
open File
open Parse_aux

%}

/* Tokens */

%token AMPER
%token AMPERAMPER
%token BANG
%token BANGEQUAL
%token BAR
%token BARBAR
%token BOOLEAN
%token BYTE
%token CARET
%token CASE
%token CHAR
%token <char> CHARACTER
%token COLON
%token COMMA
%token CONST
%token CPP_QUOTE
%token DEFAULT
%token DOT
%token DOUBLE
%token ENUM
%token EOF
%token EQUAL
%token EQUALEQUAL
%token FALSE
%token FLOAT
%token GREATER
%token GREATEREQUAL
%token GREATERGREATER
%token GREATERGREATERGREATER
%token HANDLE_T
%token HYPER
%token <string> IDENT
%token IMPORT
%token INT
%token INT64
%token INTERFACE
%token <int64> INTEGER
%token LBRACE
%token LBRACKET
%token LESS
%token LESSEQUAL
%token LESSLESS
%token LONG
%token LPAREN
%token MINUS
%token NULL
%token PERCENT
%token PLUS
%token QUESTIONMARK
%token QUOTE
%token RBRACE
%token RBRACKET
%token RPAREN
%token SEMI
%token SHORT
%token SIGNED
%token SIZEOF
%token SLASH
%token SMALL
%token STAR
%token <string> STRING
%token STRUCT
%token SWITCH
%token TILDE
%token TRUE
%token TYPEDEF
%token <string> TYPEIDENT
%token UNION
%token UNSIGNED
%token <string> UUID
%token VOID
%token WCHAR_T

/* Precedences and associativities. Lower precedences come first. */

%right QUESTIONMARK prec_conditional
%left BARBAR
%left AMPERAMPER
%left BAR
%left CARET
%left AMPER
%left EQUALEQUAL BANGEQUAL
%left LESS LESSEQUAL GREATER GREATEREQUAL
%left LESSLESS GREATERGREATER GREATERGREATERGREATER
%left PLUS MINUS
%left STAR SLASH PERCENT
%right prec_uminus BANG TILDE prec_deref prec_addressof prec_cast
%left DOT prec_dot MINUSGREATER LBRACKET prec_subscript

/* Start symbol */

%start file
%type <File.components> file

%%

/* Main entry point */

file: component_list EOF
        { List.rev $1 }
;

/* Components */

component_list:
    /*empty*/
        { [] }
  | component_list component
        { $2 @ $1 }
;

component:
    const_decl SEMI
        { [Comp_constdecl $1] }
  | type_decl SEMI
        { List.map (fun td -> Comp_typedecl td) (List.rev $1) }
  | attributes struct_declarator SEMI
        /* Attributes are ignored, they are allowed just to avoid a
           parsing ambiguity with fun_decl */
        { [Comp_structdecl $2] }
  | attributes union_declarator SEMI
        { [Comp_uniondecl $2] }
  | attributes enum_declarator SEMI
        { [Comp_enumdecl $2] }
  | attributes STRUCT opt_ident SEMI
        { [Comp_structdecl {sd_name = $3; sd_mod = "";
                            sd_stamp = 0; sd_fields = []}] }
  | attributes UNION opt_ident SWITCH LPAREN simple_type_spec ident RPAREN SEMI
        { [Comp_structdecl {sd_name = $3; sd_mod = "";
                            sd_stamp = 0; sd_fields = []}] }
  | attributes UNION opt_ident SEMI
        { [Comp_uniondecl {ud_name = $3; ud_mod = "";
                           ud_stamp = 0; ud_cases = []}] }
  | fun_decl SEMI
        { [Comp_fundecl $1] }
  | interface_attributes INTERFACE tydef_ident opt_superinterface
    LBRACE component_list RBRACE opt_semi
    /* Valid MIDL attributes: object uuid local endpoint version
           pointer_default implicit_handle auto_handle */
        { let i = make_interface $3 $1 $4 (List.rev $6) in
          restore_defaults(); i }
  | interface_attributes INTERFACE tydef_ident SEMI
        { let i = [make_forward_interface $3] in
          restore_defaults(); i }
  | IMPORT imports SEMI
        { read_imports $2 }
  | quote opt_semi
        { let (kind, txt) = make_diversion $1 in [Comp_diversion(kind, txt)] }
;

/* Import directive */

imports:
    STRING
        { [$1] }
  | imports COMMA STRING
        { $3 :: $1 }

/* Constant declaration */

const_decl:
    CONST attributes type_spec pointer_opt IDENT EQUAL lexpr
        { make_const_decl $2 ($4 $3) $5 $7 }
;
/* Typedef */

type_decl:
    /* Valid MIDL attributes: handle, switch_type, switch_is, transmit_as,
       ref, unique, ptr, context_handle, ignore, string */
    TYPEDEF attributes type_spec declarators
        { make_typedef $2 $3 (List.rev $4) }
;

/* Function declaration */

fun_decl:
    /* Valid MIDL attributes: callback, local, ref, unique, ptr, string,
       ignore, context_handle */
    attributes type_spec pointer_opt IDENT
    LPAREN param_list_declarator RPAREN opt_quotes
        { make_fun_declaration $1 ($3 $2) $4 $6 $8 }
;
opt_quotes:
    opt_quotes QUOTE LPAREN STRING RPAREN
        { ("call", $4) :: $1 }
  | opt_quotes QUOTE LPAREN ident COMMA STRING RPAREN
        { ($4, $6) :: $1 }
  | /* empty */
        { [] }
;
    
/* Parameter lists */

param_list_declarator:
    /*empty*/
        { [] }
  | VOID
        { [] }
  | param_declarators
        { List.rev $1 }
;
param_declarators:
    param_declarator
        { [$1] }
  | param_declarators COMMA param_declarator
        { $3 :: $1 }
;
param_declarator:
    /* Valid MIDL attributes: in, out, first_is, last_is,
      length_is, max_is, size_is, switch_type, switch_is, ref, unique, ptr,
      context_handle, string */
    attributes type_spec declarator
        { make_param $1 $2 $3 }
;

/* Type specifications */

type_spec:
    simple_type_spec
        { $1 }
  | STRUCT opt_ident
        { Type_struct {sd_name=$2; sd_mod = ""; sd_stamp=0; sd_fields=[]} }
  | struct_declarator
        { Type_struct $1 }
  | UNION opt_ident
        { Type_union({ud_name=$2; ud_mod = ""; ud_stamp=0; ud_cases=[]},
                      no_switch) }
  | union_declarator
        { Type_union($1, no_switch) }
  | ENUM opt_ident
        { Type_enum({en_name=$2; en_mod = ""; en_stamp=0; en_consts=[]},
                    no_enum_attr) }
  | enum_declarator
        { Type_enum($1, no_enum_attr) }
  | CONST type_spec
        { make_type_const $2 }
  | type_spec CONST
        { make_type_const $1 }
;

simple_type_spec:
    FLOAT                                       { Type_float }
  | DOUBLE                                      { Type_double }
  | INT                                         { make_int Int }
  | UNSIGNED INT                                { make_int UInt }
  | SIGNED INT                                  { make_int Int }
  | integer_size opt_int                        { make_int $1 }
  | UNSIGNED integer_size opt_int               { make_unsigned $2 }
  | integer_size UNSIGNED opt_int               { make_unsigned $1 }
  | SIGNED integer_size opt_int                 { make_signed $2 }
  | integer_size SIGNED opt_int                 { make_signed $1 }
  | CHAR                                        { make_int Char }
  | UNSIGNED CHAR                               { make_int UChar }
  | SIGNED CHAR                                 { make_int SChar }
  | BOOLEAN                                     { make_int Boolean }
  | BYTE                                        { make_int Byte }
  | INT64                                       { make_int Hyper }
  | UNSIGNED INT64                              { make_int UHyper }
  | SIGNED INT64                                { make_int Hyper }
  | VOID                                        { Type_void }
  | TYPEIDENT                                   { Type_named("", $1) }
  | WCHAR_T                                     { wchar_t_type() }
  | HANDLE_T                                    { handle_t_type() }
;
integer_size:
    LONG                                        { Long }
  | SMALL                                       { Small }
  | SHORT                                       { Short }
  | HYPER                                       { Hyper }
  | LONG LONG                                   { Hyper }
;
opt_int:
    /* nothing */                               { () }
  | INT                                         { () }
;

/* Declarators */

declarators:
    declarator
        { [$1] }
  | declarators COMMA declarator
        { $3 :: $1 }
;
declarator:
    pointer_opt direct_declarator
        { fun ty -> $2($1(ty)) }
;
pointer_opt:
    /* empty */
        { fun ty -> ty }
  | pointer_opt STAR
        { fun ty -> $1(Type_pointer(!pointer_default, ty)) }
  | pointer_opt STAR CONST
        { fun ty -> $1(Type_const(Type_pointer(!pointer_default, ty))) }
;
direct_declarator:
    ident
        { fun ty -> ($1, ty) }
  | LPAREN declarator RPAREN
        { $2 }
  | direct_declarator array_bounds_declarator
        { fun ty -> let (id, ty1) = $1 ty in (id, Type_array($2, ty1)) }
;
array_bounds_declarator:
    LBRACKET RBRACKET                           { no_bounds }
  | LBRACKET STAR RBRACKET                      { no_bounds }
  | LBRACKET lexpr RBRACKET                     { one_bound $2 }
;

/* Struct declaration and discriminated unions */

union_name:
    ident 
        { $1 }
  | /* empty */
        { "u" }
;
struct_declarator:
    STRUCT opt_ident LBRACE field_declarators RBRACE
        { {sd_name = $2; sd_mod = ""; sd_stamp = 0; sd_fields = $4} } 
  | UNION opt_ident SWITCH LPAREN simple_type_spec ident RPAREN union_name
    LBRACE union_body RBRACE
        { make_discriminated_union $2 $8 $6 $5 (List.rev $10) }
;
field_declarators:
    field_declarator
        { $1 }
  | field_declarators field_declarator
        { $1 @ $2 }
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
;
union_body:
    union_case                                          { [$1] }
  | union_body union_case                               { $2 :: $1 }
;
union_case:
    case_list opt_field_declarator SEMI
        { {case_labels = List.rev $1; case_field = $2} }
  | DEFAULT COLON opt_field_declarator SEMI
        { {case_labels = []; case_field = $3} }
;
case_list:
    case_label                                          { [$1] }
  | case_list case_label                                { $2 :: $1 }
;
case_label:
    CASE ident COLON                                    { $2 }
;
opt_field_declarator:
    /* empty */
        { None }
  | attributes type_spec declarator
  /* Valid MIDL attributes: first_is, last_is, length_is, max_is, size_is,
     string, ignore, context_handle, ref, unique, ptr, switch_type,
     switch_is */
        { Some(make_field $1 $2 $3) }
;

/* Enumerated types */

enum_declarator:
    ENUM opt_ident LBRACE enum_cases opt_comma RBRACE
         { {en_name = $2; en_mod = ""; en_stamp = 0; en_consts = List.rev $4} }
;
enum_cases:
    enum_case                                           { [$1] }
  | enum_cases COMMA enum_case                          { $3 :: $1 }
;
enum_case:
    ident
      { {const_name = $1; const_val = None} }
  | ident EQUAL lexpr
      { {const_name = $1; const_val = Some $3} }
;
opt_comma:
    COMMA                                               { () }
  | /* empty */                                         { () }
;

/* Attributes */

interface_attributes:
    attributes          { let a = $1 in save_defaults(); update_defaults a; a }
;
attributes:
    /* empty */                                         { [] }
  | LBRACKET attribute_list RBRACKET                    { List.rev $2 }
;
attribute_list:
    attribute                                           { [$1] }
  | /*empty*/                                           { [] }
  | attribute_list COMMA attribute                      { $3 :: $1 }
  | attribute_list COMMA                                { $1 }
;
attribute:
    ident
        { ($1, []) }
  | ident LPAREN attr_vars RPAREN
        { ($1, List.rev $3) }
  | STAR attribute
        { make_star_attribute $2 }
  | attribute STAR
        { make_star_attribute $1 }
  | ident UUID
        { ($1, [Expr_string $2]) }
;
attr_vars:
    attr_var
        { [$1] }
  | attr_vars COMMA attr_var
        { $3 :: $1 }
;
attr_var:
    lexpr
        { $1 }
  | /*nothing*/
        { null_attr_var }
;

/* Limited expressions */

lexpr:
    IDENT
        { Expr_ident $1 }
  | INTEGER
        { Expr_int $1 }
  | CHARACTER
        { Expr_int(Int64.of_int(Char.code $1)) }
  | TRUE
        { Expr_int Int64.one }
  | FALSE
        { Expr_int Int64.zero }
  | STRING
        { Expr_string $1 }
  | lexpr QUESTIONMARK lexpr COLON lexpr %prec prec_conditional
        { Expr_cond($1, $3, $5) }
  | lexpr BARBAR lexpr
        { Expr_sequor($1, $3) }
  | lexpr AMPERAMPER lexpr
        { Expr_sequand($1, $3) }
  | lexpr BAR lexpr
        { Expr_logor($1, $3) }
  | lexpr CARET lexpr
        { Expr_logxor($1, $3) }
  | lexpr AMPER lexpr
        { Expr_logand($1, $3) }
  | lexpr EQUALEQUAL lexpr
        { Expr_eq($1, $3) }
  | lexpr BANGEQUAL lexpr
        { Expr_ne($1, $3) }
  | lexpr LESS lexpr
        { Expr_lt($1, $3) }
  | lexpr GREATER lexpr
        { Expr_gt($1, $3) }
  | lexpr LESSEQUAL lexpr
        { Expr_le($1, $3) }
  | lexpr GREATEREQUAL lexpr
        { Expr_ge($1, $3) }
  | lexpr LESSLESS lexpr
        { Expr_lshift($1, $3) }
  | lexpr GREATERGREATER lexpr
        { Expr_rshift($1, $3) }
  | lexpr GREATERGREATERGREATER lexpr
        { Expr_rshift_unsigned($1, $3) }
  | lexpr PLUS lexpr
        { Expr_plus($1, $3) }
  | lexpr MINUS lexpr
        { Expr_minus($1, $3) }
  | lexpr STAR lexpr
        { Expr_times($1, $3) }
  | lexpr SLASH lexpr
        { Expr_div($1, $3) }
  | lexpr PERCENT lexpr
        { Expr_mod($1, $3) }
  | PLUS lexpr %prec prec_uminus
        { $2 }
  | MINUS lexpr %prec prec_uminus
        { Expr_neg($2) }
  | TILDE lexpr
        { Expr_lognot($2) }
  | BANG lexpr
        { Expr_boolnot($2) }
  | STAR lexpr %prec prec_deref
        { Expr_deref($2) }
  | AMPER lexpr %prec prec_addressof
        { Expr_addressof($2) }
  | LPAREN type_expr RPAREN lexpr %prec prec_cast
        { Expr_cast($2, $4) }
  | SIZEOF LPAREN type_expr RPAREN
        { Expr_sizeof($3) }
  | lexpr LBRACKET lexpr RBRACKET %prec prec_subscript
        { Expr_subscript($1, $3) }
  | lexpr MINUSGREATER ident
        { Expr_dereffield($1, $3) }
  | lexpr DOT ident %prec prec_dot
        { Expr_field($1, $3) }
  | lexpr DOT INTEGER %prec prec_dot
        /* This is a hack for parsing version attributes, e.g. version(0.1) */
        { Expr_field($1, Int64.to_string $3) }
  | LPAREN lexpr RPAREN
        { $2 }
;

type_expr:
    type_spec
        { $1 }
  | type_spec abstract_declarator
        { $2($1) }
;

abstract_declarator:
    STAR
        { fun ty -> Type_pointer(!pointer_default, ty) }
  | STAR direct_abstract_declarator
        { fun ty -> $2(Type_pointer(!pointer_default, ty)) }
  | direct_abstract_declarator
        { $1 }
;

direct_abstract_declarator:
    LPAREN abstract_declarator RPAREN
        { $2 }
  | direct_abstract_declarator array_bounds_declarator
        { fun ty -> Type_array($2, ty) }
;

/* Optional names for struct, union, enums */

opt_ident:
    /*empty*/                   { "" }
  | ident                       { $1 }
;

/* Optional name of superinterface for interfaces */

opt_superinterface:
    /*empty*/
        { None }
  | COLON ident
        { Some $2 }
;

/* Optional semicolon */

opt_semi:
    SEMI       { () }
  | /*empty*/  { () }
;

/* Any ident (type or not) */

ident:
    IDENT
        { $1 }
  | TYPEIDENT
        { $1 }
;

/* An ident that becomes a type name */

tydef_ident:
    ident
        { type_names := StringSet.add $1 !type_names; $1 }
;

/* Quotes (diversions) */

quote:
    QUOTE LPAREN STRING RPAREN
        { ("", $3) }
  | QUOTE LPAREN ident COMMA STRING RPAREN
        { ($3, $5) }
  | CPP_QUOTE LPAREN STRING RPAREN
        { ("h", $3) }
;

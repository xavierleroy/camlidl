/* Parser for a home-made IDL (a subset of DCE IDL) */

%{

open Printf
open Idltypes

let null_attr_var = Var ""

let no_bounds = { bound = None; size = None; length = None; is_string = false }

let one_bound n = { no_bounds with bound = Some n }

let no_ptrattr = { ptrkind = Ref; ignore = false }

let no_switch = null_attr_var

(* Apply a type-related attribute to a type *)

let rec merge_array_attr merge_fun rexps ty =
  match (rexps, ty) with
    ([], _) -> ty
  | (re :: rem, Type_array(attr, ty_elt)) ->
      let attr' = if re == null_attr_var then attr else merge_fun attr re in
      Type_array(attr', merge_array_attr merge_fun rem ty_elt)
  | (_, _) ->
      eprintf "Warning: size_is or length_is attribute applied to \
               non-array, ignored.\n";
      ty

let apply_type_attribute ty attr =
  match (attr, ty) with
    (("ref", _), Type_pointer(attr, ty_elt)) ->
      Type_pointer({attr with ptrkind = Ref}, ty_elt)
  | (("unique", _), Type_pointer(attr, ty_elt)) ->
      Type_pointer({attr with ptrkind = Unique}, ty_elt)
  | (("ptr", _), Type_pointer(attr, ty_elt)) ->
      Type_pointer({attr with ptrkind = Ptr}, ty_elt)
  | (("ignore", _), Type_pointer(attr, ty_elt)) ->
      Type_pointer({attr with ignore = true}, ty_elt)
  | (("string", _), Type_array(attr, (Type_int(Char|UChar|Byte) as ty_elt))) ->
      Type_array({attr with is_string = true}, ty_elt)
  | (("string", _), Type_pointer(attr, (Type_int(Char|UChar|Byte) as ty_elt))) ->
      Type_array({no_bounds with is_string = true}, ty_elt)
  | (("size_is", rexps), Type_array(_, _)) ->
      merge_array_attr (fun attr re -> {attr with size = Some re})
                       rexps ty
  | (("length_is", rexps), Type_array(_, _)) ->
      merge_array_attr (fun attr re -> {attr with length = Some re})
                       rexps ty
  | (("switch_is", [rexp]), Type_union(name, discr)) ->
      Type_union(name, rexp)
  | ((name, _), _) ->
      eprintf
        "Warning: attribute `%s' unknown or not applicable here, ignored.\n"
        name;
      ty

let apply_type_attributes = List.fold_left apply_type_attribute

let make_param attrs tybase decl =
  let (name, ty) = decl tybase in
  let rec merge_attributes mode ty = function
    [] ->
      let real_mode = match mode with None -> In | Some m -> m in
      (name, real_mode, ty)
  | ("in", _) :: rem ->
      let mode' =
        match mode with Some InOut -> mode
                      | Some Out -> Some InOut
                      | _ -> Some In in
      merge_attributes mode' ty rem
  | ("out", _) :: rem ->
      let mode' =
        match mode with Some InOut -> mode
                      | Some In -> Some InOut
                      | _ -> Some Out in
      merge_attributes mode' ty rem
  | attr :: rem ->
      merge_attributes mode (apply_type_attribute ty attr) rem in
  merge_attributes None ty attrs

let make_op_declaration attrs ty_res name params =
  { fun_name = name;
    fun_res = apply_type_attributes ty_res attrs;
    fun_params = params }

let make_fields attrs tybase decls =
  List.map
    (fun decl ->
      let (name, ty) = decl tybase in
      { field_name = name; field_typ = apply_type_attributes ty attrs })
    decls

let make_typedef attrs tybase decls =
  List.map
    (fun decl ->
      let (name, ty) = decl tybase in
      {td_name = name; td_type = apply_type_attributes ty attrs})
    decls

(* Apply an "unsigned" modifier to an integer type *)

let make_unsigned = function
    Type_int kind ->
      Type_int(match kind with Int -> UInt | Long -> ULong | Small -> USmall
                             | Short -> UShort | Char -> UChar | k -> k)
  | ty -> ty

(* Provide names for anonymous struct, union, enum *)

let anonymous_decl_counter = ref 0

(* Record struct, union, enum declared in the middle of a type expression *)

let extra_components = ref ([] : interface_component list)

let add_comp c = extra_components := c :: !extra_components

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
%token <string> IDENT
%token INT
%token <int> INTEGER
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
%token RBRACE
%token RBRACKET
%token RPAREN
%token SEMI
%token SHORT
%token SLASH
%token SMALL
%token STAR
%token <string> STRING
%token STRUCT
%token TILDE
%token TRUE
%token TYPEDEF
%token UNION
%token UNSIGNED
%token VOID

/* Precedences and associativities. Lower precedences come first. */

%right QUESTIONMARK prec_conditional
%left BARBAR
%left AMPERAMPER
%left BAR
%left CARET
%left AMPER
%left EQUALEQUAL BANGEQUAL
%left LESS LESSEQUAL GREATER GREATEREQUAL
%left LESSLESS GREATERGREATER
%left PLUS MINUS
%left STAR SLASH PERCENT
%right prec_uminus BANG TILDE

/* Start symbol */

%start file
%type <Idltypes.interface_component list> file

%%

/* Main entry point */

file: interface_components EOF  { List.rev ($1 @ !extra_components) }
;

/* Components */

interface_components:
    /* empty */                                 { [] }
  | interface_components interface_component    { $2 :: $1 }
;
interface_component:
    type_declarator SEMI                        { Comp_typedecl $1 }
  | struct_declarator SEMI                      { Comp_structdecl $1 }
  | union_declarator SEMI                       { Comp_uniondecl $1 }
  | enum_declarator SEMI                        { Comp_enumdecl $1 }
  | op_declarator SEMI                          { Comp_fundecl $1 }
;

/* Type declaration */

type_declarator:
    TYPEDEF attributes type_spec declarators
                                        { make_typedef $2 $3 (List.rev $4) }
;              

/* Type specifications */

type_spec:
    simple_type_spec                    { $1 }
  | STRUCT IDENT                        { Type_struct $2 }
  | struct_declarator                   { add_comp(Comp_structdecl $1);
                                          Type_struct $1.sd_name }
  | UNION IDENT                         { Type_union($2, no_switch) }
  | union_declarator                    { add_comp(Comp_uniondecl $1);
                                          Type_union($1.ud_name, no_switch) }
  | ENUM IDENT                          { Type_enum $2 }
  | enum_declarator                     { add_comp(Comp_enumdecl $1);
                                          Type_enum $1.en_name }
;
simple_type_spec:
    FLOAT                                       { Type_float }
  | DOUBLE                                      { Type_double }
  | INT                                         { Type_int Int }
  | UNSIGNED INT                                { Type_int UInt }
  | integer_size opt_int                        { $1 }
  | UNSIGNED integer_size opt_int               { make_unsigned $2 }
  | integer_size UNSIGNED opt_int               { make_unsigned $1 }
  | CHAR                                        { Type_int Char }
  | UNSIGNED CHAR                               { Type_int UChar }
  | BOOLEAN                                     { Type_int Boolean }
  | BYTE                                        { Type_int Byte }
  | VOID                                        { Type_void }
  | IDENT                                       { Type_named $1 }
;
integer_size:
    LONG                                        { Type_int Long }
  | SMALL                                       { Type_int Small }
  | SHORT                                       { Type_int Short }
;
opt_int:
    /* nothing */                               { () }
  | INT                                         { () }
;

/* Struct and union declarators */

struct_declarator:
    STRUCT opt_ident LBRACE field_declarators RBRACE
                        { {sd_name = $2; sd_fields = $4} } 
;
field_declarators:
    field_declarator                            { $1 }
  | field_declarators field_declarator          { $1 @ $2 }
;
field_declarator:
    attributes type_spec declarators SEMI
                        { make_fields $1 $2 (List.rev $3) }
;

union_declarator:
  | UNION opt_ident LBRACE union_body RBRACE
                        { {ud_name = $2; ud_cases = List.rev $4} }
;
union_body:
    union_case                                          { [$1] }
  | union_body union_case                               { $2 :: $1 }
;
union_case:
    case_list opt_field_declarator SEMI
                        { {case_labels = List.rev $1; case_fields = $2} }
  | DEFAULT COLON opt_field_declarator SEMI
                        { {case_labels = []; case_fields = $3} }
;
case_list:
    case_label                                          { [$1] }
  | case_list case_label                                { $2 :: $1 }
;
case_label:
    CASE IDENT COLON                                    { $2 }
;
opt_field_declarator:
    /* empty */                                         { [] }
  | field_declarator                                    { $1 }
;

/* Enumerated types */

enum_declarator:
    ENUM opt_ident LBRACE enum_cases RBRACE
                          { {en_name = $2; en_consts = List.rev $4} }
;
enum_cases:
    enum_case                                           { [$1] }
  | enum_cases COMMA enum_case                          { $3 :: $1 }
;
enum_case:
    IDENT                                               { $1 }
  | IDENT EQUAL INT                                     { $1 }
;

/* Declarators */

declarators:
    declarator                                  { [$1] }
  | declarators COMMA declarator                { $3 :: $1 }
;
declarator:
    pointer_opt direct_declarator               { fun ty -> $2($1(ty)) }
;
pointer_opt:
    /* empty */                     { fun ty -> ty }
  | pointer_opt STAR                { fun ty -> Type_pointer(no_ptrattr, ty) }
;
direct_declarator:
    IDENT                                       { fun ty -> ($1, ty) }
  | LPAREN declarator RPAREN                    { $2 }
  | direct_declarator array_bounds_declarator
      { fun ty -> let (id, ty1) = $1 ty in (id, Type_array($2, ty1)) }
;
array_bounds_declarator:
    LBRACKET RBRACKET                           { no_bounds }
  | LBRACKET STAR RBRACKET                      { no_bounds }
  | LBRACKET INTEGER RBRACKET                   { one_bound $2 }
;

/* Attributes */

attributes:
    /* empty */                                 { [] }
  | LBRACKET attribute_list RBRACKET            { List.rev $2 }
;
attribute_list:
    attribute                                   { [$1] }
  | attribute_list COMMA attribute              { $3 :: $1 }
;
attribute:
    IDENT                                       { ($1, []) }
  | IDENT LPAREN attr_vars RPAREN               { ($1, List.rev $3) }
;
attr_vars:
    attr_var                                    { [$1] }
  | attr_vars COMMA attr_var                    { $3 :: $1 }
;
attr_var:
    IDENT                                       { Var $1 }
  | STAR IDENT                                  { Deref $2 }
  | /*nothing*/                                 { null_attr_var }
;

/* Operations */

op_declarator:
    attributes simple_type_spec pointer_opt IDENT
    LPAREN param_list_declarator RPAREN
      { make_op_declaration $1 ($3 $2) $4 $6 }
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

/* Optional idents for struct, union, enum names */

opt_ident:
    IDENT                       { $1 }
  | /*empty*/                   { incr anonymous_decl_counter;
                                  string_of_int !anonymous_decl_counter }
;

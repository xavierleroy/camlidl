/* Parser for Microsoft IDL */

%{

open Printf
open Idltypes
open Funct
open Typedef
open Constdecl
open Intf
open File

let null_attr_var = Var ""

let no_bounds =
  { bound = None; size = None; length = None;
    is_string = false; null_terminated = false }

let one_bound n = { no_bounds with bound = Some n }

let no_switch = { discriminant = null_attr_var }

let no_enum_attr = { bitset = false }

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

let is_star_attribute name = String.length name >= 1 && name.[0] = '*'
let star_attribute name = String.sub name 1 (String.length name - 1)

let rec apply_type_attribute ty attr =
  match (attr, ty) with
    (("ref", _), Type_pointer(attr, ty_elt)) ->
      Type_pointer(Ref, ty_elt)
  | (("unique", _), Type_pointer(attr, ty_elt)) ->
      Type_pointer(Unique, ty_elt)
  | (("ptr", _), Type_pointer(attr, ty_elt)) ->
      Type_pointer(Ptr, ty_elt)
  | (("ignore", _), Type_pointer(attr, ty_elt)) ->
      Type_pointer(Ignore, ty_elt)
  | (("string", _), Type_array(attr, (Type_int(Char|UChar|Byte) as ty_elt))) ->
      Type_array({attr with is_string = true}, ty_elt)
  | (("string", _), Type_pointer(attr, (Type_int(Char|UChar|Byte) as ty_elt))) ->
      Type_array({no_bounds with is_string = true}, ty_elt)
  | (("null_terminated", _), Type_array(attr, ty_elt))->
      Type_array({attr with null_terminated = true}, ty_elt)
  | (("null_terminated", _), Type_pointer(attr, ty_elt)) ->
      Type_array({no_bounds with null_terminated = true}, ty_elt)
  | (("size_is", rexps), Type_array(_, _)) ->
      merge_array_attr (fun attr re -> {attr with size = Some re})
                       rexps ty
  | (("length_is", rexps), Type_array(_, _)) ->
      merge_array_attr (fun attr re -> {attr with length = Some re})
                       rexps ty
  | (("switch_is", [rexp]), Type_union(name, attr)) ->
      Type_union(name, {attr with discriminant = rexp})
  | (("switch_is", [rexp]), Type_pointer(attr, Type_union(name, attr'))) ->
      Type_pointer(attr, Type_union(name, {attr' with discriminant = rexp}))
  | (("set", _), Type_enum(name, attr)) ->
      Type_enum(name, {attr with bitset = true})
  | ((("context_handle" | "switch_type"), _), _) ->
      ty (*ignored*)
  | ((name, rexps), Type_pointer(attr, ty_elt)) when is_star_attribute name ->
      Type_pointer(attr,
                   apply_type_attribute ty_elt (star_attribute name, rexps))
  | ((name, rexps), Type_array(attr, ty_elt)) when is_star_attribute name ->
      Type_array(attr,
                 apply_type_attribute ty_elt (star_attribute name, rexps))
  | ((name, _), _) ->
      eprintf
        "Warning: attribute `%s' unknown, malformed or not applicable here, \
         ignored.\n"
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

let make_fun_declaration attrs ty_res name params diversion =
  let rec merge_attributes ty = function
      [] -> ty
    | (("callback" | "local"), _) :: rem -> merge_attributes ty rem
    | attr :: rem -> merge_attributes (apply_type_attribute ty attr) rem in
  { fun_name = name;
    fun_mod = "";
    fun_res = merge_attributes ty_res attrs;
    fun_params = params;
    fun_call = diversion }

let make_fields attrs tybase decls =
  List.map
    (fun decl ->
      let (name, ty) = decl tybase in
      { field_name = name; field_typ = apply_type_attributes ty attrs })
    decls

let make_field attrs tybase decl =
  let (name, ty) = decl tybase in
  { field_name = name; field_typ = apply_type_attributes ty attrs }

let make_discriminated_union name switch_name switch_type body =
  let ty_union =
    Type_union({ud_name = ""; ud_mod = ""; ud_stamp = 0; ud_cases = body},
               {discriminant = Var switch_name}) in
  { sd_name = name; sd_mod = ""; sd_stamp = 0;
    sd_fields = [ {field_name = switch_name; field_typ = switch_type};
                  {field_name = "u"; field_typ = ty_union} ] }

let make_typedef attrs tybase decls =
  let rec merge_attributes ty td = function
    [] -> (ty, td)
  | ("abstract", _) :: rem ->
      merge_attributes ty {td with td_abstract = true} rem
  | ("c2ml", [Var f]) :: rem ->
      merge_attributes ty {td with td_c2ml = Some f} rem
  | ("ml2c", [Var f]) :: rem ->
      merge_attributes ty {td with td_ml2c = Some f} rem
  | ("mltype", [Var f]) :: rem ->
      merge_attributes ty {td with td_mltype = Some f} rem
  | ("errorcode", _) :: rem ->
      merge_attributes ty {td with td_errorcode = true} rem
  | ("errorcheck", [Var f]) :: rem ->
      merge_attributes ty {td with td_errorcheck = Some f} rem
  | (("handle" | "transmit_as" | "context_handle"), _) :: rem ->
      merge_attributes ty td rem
  | attr :: rem ->
      merge_attributes (apply_type_attribute ty attr) td rem in
  List.map
    (fun decl ->
      let (name, ty) = decl tybase in
      let td = {td_name = name; td_mod = "";
                td_type = Type_void; (* dummy *)
                td_abstract = false; td_mltype = None;
                td_c2ml = None; td_ml2c = None;
                td_errorcode = false; td_errorcheck = None} in
      let (ty', td') = merge_attributes ty td attrs in
      {td' with td_type = ty'})
    decls

let make_interface name attrs superintf imports comps =
  let obj = ref false in
  let uid = ref "" in
  let ptrdef = ref Unique in
  let parse_attr = function
    ("object", _) -> obj := true
  | ("uuid", [Var u]) -> uid := u
  | ("pointer_default", [Var "ref"]) -> ptrdef := Ref
  | ("pointer_default", [Var "unique"]) -> ptrdef := Unique
  | ("pointer_default", [Var "ptr"]) -> ptrdef := Ptr
  | ("local", _) -> () (*ignored*)
  | ("endpoint", _) -> () (*ignored*)
  | ("version", _) -> () (*ignored*)
  | ("implicit_handle", _) -> () (*ignored*)
  | ("auto_handle", _) -> () (*ignored*)
  | (name, _) ->
        eprintf "Warning: attribute `%s' unknown, malformed or not \
                 applicable here, ignored.\n" name in
  List.iter parse_attr attrs;
  let supername =
    match superintf with
      None ->
        if not !obj then "" else begin
          eprintf "Warning: no super-interface for interface `%s', \
                   assuming IUnknown.\n"
                  name;
          "IUnknown"
        end
    | Some s ->
        if !obj then s else begin
          eprintf "Warning: interface `%s' is not an object interface, \
                   ignoring super-interface `%s'.\n"
                  name s;
          ""
        end in
  { iif_name = name;
    iif_imports = imports;
    iif_comps = comps;
    iif_super = supername;
    iif_obj = !obj;
    iif_uid = !uid;
    iif_ptr_default = !ptrdef }  

let make_diversion (id, txt) =
  let kind =
    match id with
      "" | "c" -> Div_c
    | "ml" -> Div_ml
    | "mli" -> Div_mli
    | "mlmli" -> Div_ml_mli
    | _ ->
      eprintf "Warning: diversion kind `%s' unknown, assuming C kind.\n" id;
      Div_c in
  (kind, txt)

(* Apply an "unsigned" or "signed" modifier to an integer type *)

let make_unsigned kind =
  Type_int(match kind with
             Int -> UInt | Long -> ULong | Small -> USmall
           | Short -> UShort | Char -> UChar | SChar -> UChar
           | k -> k)

let make_signed kind =
  Type_int(match kind with
             UInt -> Int | ULong -> Long | USmall -> Small
           | UShort -> Short | Char -> SChar | UChar -> SChar
           | k -> k)

(* Warn about the handle_t type *)

let handle_t_type() =
  eprintf
    "Warning: type `handle_t' unsupported, treating as an opaque pointer.\n";
  Type_pointer(Ptr, Type_int Int)

(* Warn about the hyper type *)

let hyper_type() =
  eprintf "Warning: type `hyper' unsupported, treating as `long'.\n";
  Long

(* Apply a "star" modifier to an attribute *)

let make_star_attribute (name, args) = ("*" ^ name, args)

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
%token <string * string> DIVERSION
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
%token HANDLE_T
%token HYPER
%token <string> IDENT
%token IMPORT
%token INT
%token INTERFACE
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
%token SIGNED
%token SLASH
%token SMALL
%token STAR
%token <string> STRING
%token STRUCT
%token SWITCH
%token TILDE
%token TRUE
%token TYPEDEF
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
%left LESSLESS GREATERGREATER
%left PLUS MINUS
%left STAR SLASH PERCENT
%right prec_uminus BANG TILDE

/* Start symbol */

%start file
%type <File.idl_intf list> file

%%

/* Main entry point */

file: interface_list EOF
        { List.rev $1 }
;

interface_list:
    /*empty*/
        { [] }
  | interface_list interface
        { $2 :: $1 }
;

interface:
    /* Valid MIDL attributes: object uuid local endpoint version
       pointer_default implicit_handle auto_handle */
    attributes INTERFACE IDENT opt_superinterface
    LBRACE import_list component_list RBRACE
        { make_interface $3 $1 $4 (List.rev $6) (List.rev $7) }
  | attributes INTERFACE IDENT SEMI
        { make_interface $3 $1 None [] [] }
;

opt_superinterface:
    /*empty*/
        { None }
  | COLON IDENT
        { Some $2 }
;

/* Import list */

import_list:
    /* empty */
        { [] }
  | import_list IMPORT name_list SEMI
        { $3 @ $1 }
;

name_list:
    STRING
        { [$1] }
  | name_list COMMA STRING
        { $3 :: $1 }
;

/* Interface components */

component_list:
    /*empty*/
        { [] }
  | component_list component
        { $2 @ $1 }
;

component:
    CPP_QUOTE LPAREN STRING RPAREN
        { [] }
  | const_decl SEMI
        { [Comp_constdecl $1] }
  | type_decl SEMI
        { List.map (fun td -> Comp_typedecl td) $1 }
  | attributes struct_declarator SEMI
        /* Attributes are ignored, they are allowed just to avoid a
           parsing ambiguity with fun_decl */
        { [Comp_structdecl $2] }
  | attributes union_declarator SEMI
        { [Comp_uniondecl $2] }
  | attributes enum_declarator SEMI
        { [Comp_enumdecl $2] }
  | fun_decl SEMI
        { [Comp_fundecl $1] }
  | DIVERSION
        { let (kind, txt) = make_diversion $1 in [Comp_diversion(kind, txt)] }
;

/* Constant declaration */

const_decl:
    CONST simple_type_spec pointer_opt IDENT EQUAL const_exp
        { {cd_name = $4; cd_type = $3($2); cd_value = $6} }
;

const_exp:
    const_int
        { Cst_int $1 }
  | STRING
        { Cst_string $1 }
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
    LPAREN param_list_declarator RPAREN opt_diversion
        { make_fun_declaration $1 ($3 $2) $4 $6 $8 }
;
opt_diversion:
    DIVERSION
        { let (id, txt) = $1 in Some txt }
  | /*empty*/
        { None }
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
  | STRUCT IDENT
        { Type_struct {sd_name=$2; sd_mod = ""; sd_stamp=0; sd_fields=[]} }
  | struct_declarator
        { Type_struct $1 }
  | UNION IDENT
        { Type_union({ud_name=$2; ud_mod = ""; ud_stamp=0; ud_cases=[]},
                      no_switch) }
  | union_declarator
        { Type_union($1, no_switch) }
  | ENUM IDENT
        { Type_enum({en_name=$2; en_mod = ""; en_stamp=0; en_consts=[]},
                    no_enum_attr) }
  | enum_declarator
        { Type_enum($1, no_enum_attr) }
;

simple_type_spec:
    FLOAT                                       { Type_float }
  | DOUBLE                                      { Type_double }
  | INT                                         { Type_int Int }
  | UNSIGNED INT                                { Type_int UInt }
  | SIGNED INT                                  { Type_int UInt }
  | integer_size opt_int                        { Type_int $1 }
  | UNSIGNED integer_size opt_int               { make_unsigned $2 }
  | integer_size UNSIGNED opt_int               { make_unsigned $1 }
  | SIGNED integer_size opt_int                 { make_signed $2 }
  | integer_size SIGNED opt_int                 { make_signed $1 }
  | CHAR                                        { Type_int Char }
  | UNSIGNED CHAR                               { Type_int UChar }
  | SIGNED CHAR                                 { Type_int SChar }
  | BOOLEAN                                     { Type_int Boolean }
  | BYTE                                        { Type_int Byte }
  | VOID                                        { Type_void }
  | IDENT                                       { Type_named("", $1) }
  | WCHAR_T                                     { Type_int WChar }
  | HANDLE_T                                    { handle_t_type() }
;
integer_size:
    LONG                                        { Long }
  | SMALL                                       { Small }
  | SHORT                                       { Short }
  | HYPER                                       { hyper_type() }
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
        { fun ty -> Type_pointer(Default, ty) }
;
direct_declarator:
    IDENT
        { fun ty -> ($1, ty) }
  | LPAREN declarator RPAREN
        { $2 }
  | direct_declarator array_bounds_declarator
        { fun ty -> let (id, ty1) = $1 ty in (id, Type_array($2, ty1)) }
;
array_bounds_declarator:
    LBRACKET RBRACKET                           { no_bounds }
  | LBRACKET STAR RBRACKET                      { no_bounds }
  | LBRACKET const_int RBRACKET                 { one_bound $2 }
;

/* Struct declaration and discriminated unions */

struct_declarator:
    STRUCT opt_ident LBRACE field_declarators RBRACE
        { {sd_name = $2; sd_mod = ""; sd_stamp = 0; sd_fields = $4} } 
  | UNION opt_ident SWITCH LPAREN simple_type_spec IDENT RPAREN
    LBRACE union_body RBRACE
        { make_discriminated_union $2 $6 $5 (List.rev $9) }
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
    ENUM opt_ident LBRACE enum_cases RBRACE
         { {en_name = $2; en_mod = ""; en_stamp = 0; en_consts = List.rev $4} }
;
enum_cases:
    enum_case                                           { [$1] }
  | enum_cases COMMA enum_case                          { $3 :: $1 }
;
enum_case:
    IDENT                                               { $1 }
  | IDENT EQUAL const_int                               { $1 }
;

/* Attributes */

attributes:
    /* empty */                                         { [] }
  | LBRACKET attribute_list RBRACKET                    { List.rev $2 }
;
attribute_list:
    attribute                                           { [$1] }
  | attribute_list COMMA attribute                      { $3 :: $1 }
;
attribute:
    IDENT
        { ($1, []) }
  | IDENT LPAREN attr_vars RPAREN
        { ($1, List.rev $3) }
  | STAR attribute
        { make_star_attribute $2 }
  | attribute STAR
        { make_star_attribute $1 }
  | IDENT UUID
        { ($1, [Var $2]) }
;
attr_vars:
    attr_var
        { [$1] }
  | attr_vars COMMA attr_var
        { $3 :: $1 }
;
attr_var:
    IDENT
        { Var $1 }
  | STRING
        { Var $1 }
  | STAR IDENT
        { Deref $2 }
  | /*nothing*/
        { null_attr_var }
;

/* Integer constants */

const_int:
    INTEGER
        { $1 }
  | CHARACTER
        { Char.code $1 }
  | TRUE
        { 1 }
  | FALSE
        { 0 }
  | const_int QUESTIONMARK const_int COLON const_int %prec prec_conditional
        { if $1 <> 0 then $3 else $5 }
  | const_int BARBAR const_int
        { if $1 <> 0 then $1 else $3 }
  | const_int AMPERAMPER const_int
        { if $1 = 0 then 0 else $3 }
  | const_int BAR const_int
        { $1 land $3 }
  | const_int AMPER const_int
        { $1 lor $3 }
  | const_int CARET const_int
        { $1 lxor $3 }
  | const_int EQUALEQUAL const_int
        { if $1 = $3 then 1 else 0 }
  | const_int BANGEQUAL const_int
        { if $1 <> $3 then 1 else 0 }
  | const_int LESS const_int
        { if $1 < $3 then 1 else 0 }
  | const_int GREATER const_int
        { if $1 > $3 then 1 else 0 }
  | const_int LESSEQUAL const_int
        { if $1 <= $3 then 1 else 0 }
  | const_int GREATEREQUAL const_int
        { if $1 >= $3 then 1 else 0 }
  | const_int LESSLESS const_int
        { $1 lsl $3 }
  | const_int GREATERGREATER const_int
        { $1 asr $3 }
  | const_int PLUS const_int
        { $1 + $3 }
  | const_int MINUS const_int
        { $1 - $3 }
  | const_int STAR const_int
        { $1 * $3 }
  | const_int SLASH const_int
        { if $3 = 0 then 0 else $1 / $3 }
  | const_int PERCENT const_int
        { if $3 = 0 then 0 else $1 mod $3 }
  | PLUS const_int %prec prec_uminus
        { $2 }
  | MINUS const_int %prec prec_uminus
        { - $2 }
  | TILDE const_int
        { lnot $2 }
  | BANG const_int
        { if $2 = 0 then 1 else 0 }
  | LPAREN const_int RPAREN
        { $2 }
;

/* Optional names for struct, union, enums */

opt_ident:
    /*empty*/                   { "" }
  | IDENT                       { $1 }
;

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

let null_attr_var = Expr_string ""

let no_bounds =
  { bound = None; size = None; length = None;
    is_string = false; null_terminated = false }

let one_bound n = { no_bounds with bound = Some n }

let no_switch = { discriminant = null_attr_var }

let no_enum_attr = { bitset = false }

let default_ptrkind = Unique (* as per the MIDL specs *)

let pointer_default = ref default_ptrkind

(* Apply a type-related attribute to a type *)

let rec merge_array_attr merge_fun rexps ty =
  match (rexps, ty) with
    ([], _) -> ty
  | (re :: rem, Type_array(attr, ty_elt)) ->
      let attr' =
        if re == null_attr_var then attr else merge_fun attr re in
      Type_array(attr', merge_array_attr merge_fun rem ty_elt)
  | (re :: rem, Type_pointer(kind, ty_elt)) ->
      if re == null_attr_var then
        Type_pointer(kind, merge_array_attr merge_fun rem ty_elt)
      else
        Type_array(merge_fun no_bounds re,
                   merge_array_attr merge_fun rem ty_elt)
  | (_, _) ->
      eprintf "Warning: size_is or length_is attribute applied to \
               type `%a', ignored.\n" out_c_type ty;
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
  | (("size_is", rexps), (Type_array(_, _) | Type_pointer(_, _))) ->
      merge_array_attr (fun attr re -> {attr with size = Some re})
                       rexps ty
  | (("length_is", rexps), (Type_array(_, _) | Type_pointer(_, _))) ->
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
      let ty' = 
        match ty with Type_pointer(_, ty_elt) -> Type_pointer(Ref, ty_elt)
                    | _ -> ty in
      merge_attributes mode' ty' rem
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
               {discriminant = Expr_ident switch_name}) in
  { sd_name = name; sd_mod = ""; sd_stamp = 0;
    sd_fields = [ {field_name = switch_name; field_typ = switch_type};
                  {field_name = "u"; field_typ = ty_union} ] }

let make_typedef attrs tybase decls =
  let rec merge_attributes ty td = function
    [] -> (ty, td)
  | ("abstract", _) :: rem ->
      merge_attributes ty {td with td_abstract = true} rem
  | ("c2ml", [Expr_ident f]) :: rem ->
      merge_attributes ty {td with td_c2ml = Some f} rem
  | ("ml2c", [Expr_ident f]) :: rem ->
      merge_attributes ty {td with td_ml2c = Some f} rem
  | ("mltype", [Expr_ident f]) :: rem ->
      merge_attributes ty {td with td_mltype = Some f} rem
  | ("errorcode", _) :: rem ->
      merge_attributes ty {td with td_errorcode = true} rem
  | ("errorcheck", [Expr_ident f]) :: rem ->
      merge_attributes ty {td with td_errorcheck = Some f} rem
  | (("handle" | "transmit_as" | "context_handle"), _) :: rem ->
      merge_attributes ty td rem
  | attr :: rem ->
      merge_attributes (apply_type_attribute ty attr) td rem in
  let merge_definition tybase decl =
    let (name, ty) = decl tybase in
    let td = {td_name = name; td_mod = "";
              td_type = Type_void; (* dummy *)
              td_abstract = false; td_mltype = None;
              td_c2ml = None; td_ml2c = None;
              td_errorcode = false; td_errorcheck = None} in
    let (ty', td') = merge_attributes ty td attrs in
    {td' with td_type = ty'} in
  (* If one of the decls is just a name, generate it first,
     then use it as the tybase for the others decls.
     This helps for typedef struct {...} t, *p, ... *)
  let rec split_decls past = function
    [] -> (* didn't find a name, use original decls *)
      List.map (merge_definition tybase) (List.rev past)
  | decl :: rem ->
      match decl (Type_named("%", "%")) with
        (name, Type_named("%", "%")) ->
        (* Found a name, define it first, and define the others in terms
           of this name *)
          merge_definition tybase decl ::
          List.map (merge_definition (Type_named("", name)))
                   (List.rev past @ rem)
      | (_, _) ->
          split_decls (decl :: past) rem in
  split_decls [] decls

let update_pointer_default attrs =
  List.iter
    (function
        ("pointer_default", [Expr_ident "ref"]) -> pointer_default := Ref
      | ("pointer_default", [Expr_ident "unique"]) -> pointer_default := Unique
      | ("pointer_default", [Expr_ident "ptr"]) -> pointer_default := Ptr
      | _ -> ())
    attrs

let make_interface name attrs superintf imports comps =
  let obj = ref false in
  let uid = ref "" in
  let parse_attr = function
    ("object", _) -> obj := true
  | ("uuid", [Expr_string u]) -> uid := u
  | ("pointer_default", _) -> () (*treated elsewhere*)
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
  pointer_default := default_ptrkind;
  if not !obj then
    (imports, comps)
  else begin
    (* This is an object interface: split into methods and other definitions,
       lift the definitions out, build an interface from the methods *)
    let rec split_comps = function
        [] -> ([], [])
      | Comp_fundecl fd :: rem ->
          let (m, o) = split_comps rem in (fd :: m, o)
      | comp :: rem ->
          let (m, o) = split_comps rem in (m, comp :: o) in
    let (methods, others) =
      split_comps comps in
    let rec super = (* dummy super interface, only intf_name is used *)
      { intf_name = supername; intf_mod = ""; intf_super = super;
        intf_methods = []; intf_uid = "" } in
    let intf_forward =
      { intf_name = name; intf_mod = ""; intf_super = super;
        intf_methods = []; intf_uid = "" } in
    let intf =
      { intf_name = name; intf_mod = ""; intf_super = super;
        intf_methods = methods; intf_uid = !uid } in
    (imports,
     Comp_interface intf :: others @ [Comp_interface intf_forward])
  end

let make_forward_interface name =
  let rec intf =
    { intf_name = name; intf_mod = ""; intf_super = intf;
      intf_methods = []; intf_uid = "" } in
  Comp_interface intf

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

(* Warn about the wchar_t type *)

let wchar_t_type() =
  eprintf "Warning: type `wchar_t' unsupported, treating as `char'.\n";
  Type_int Char

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
%right prec_uminus BANG TILDE prec_deref prec_addressof prec_cast
%left DOT prec_dot MINUSGREATER LBRACKET prec_subscript

/* Start symbol */

%start file
%type <string list * File.components> file

%%

/* Main entry point */

file: component_list EOF
        { let (i, c) = $1 in (List.rev i, List.rev c) }
;

/* Components */

component_list:
    /*empty*/
        { [], [] }
  | component_list component
        { let (il, cl) = $1 and (i, c) = $2 in (i @ il, c @ cl) }
;

component:
    CPP_QUOTE LPAREN STRING RPAREN
        { [], [] }
  | const_decl SEMI
        { [], [Comp_constdecl $1] }
  | type_decl SEMI
        { [], List.map (fun td -> Comp_typedecl td) (List.rev $1) }
  | attributes struct_declarator SEMI
        /* Attributes are ignored, they are allowed just to avoid a
           parsing ambiguity with fun_decl */
        { [], [Comp_structdecl $2] }
  | attributes union_declarator SEMI
        { [], [Comp_uniondecl $2] }
  | attributes enum_declarator SEMI
        { [], [Comp_enumdecl $2] }
  | fun_decl SEMI
        { [], [Comp_fundecl $1] }
  | attributes INTERFACE IDENT opt_superinterface
    LBRACE component_list RBRACE
    /* Valid MIDL attributes: object uuid local endpoint version
           pointer_default implicit_handle auto_handle */
        { let (imports, comps) = $6 in make_interface $3 $1 $4 imports comps }
  | attributes INTERFACE IDENT SEMI
        { [], [make_forward_interface $3] }
  | IMPORT import_list SEMI
        { List.rev $2, [] }
  | DIVERSION
        { let (kind, txt) = make_diversion $1 in
          [], [Comp_diversion(kind, txt)] }
;

/* Constant declaration */

const_decl:
    CONST simple_type_spec pointer_opt IDENT EQUAL lexpr
        { {cd_name = $4; cd_type = $3($2); cd_value = $6} }
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
  | WCHAR_T                                     { wchar_t_type() }
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
        { fun ty -> $1(Type_pointer(!pointer_default, ty)) }
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
  | LBRACKET lexpr RBRACKET                     { one_bound $2 }
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
  | IDENT EQUAL lexpr                                   { $1 }
;

/* Attributes */

attributes:
    /* empty */
        { [] }
  | LBRACKET attribute_list RBRACKET
        { let a = List.rev $2 in update_pointer_default a; a }
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
        { Expr_int(Char.code $1) }
  | TRUE
        { Expr_int 1 }
  | FALSE
        { Expr_int 0 }
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
  | lexpr MINUSGREATER IDENT
        { Expr_dereffield($1, $3) }
  | lexpr DOT IDENT %prec prec_dot
        { Expr_field($1, $3) }
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
  | IDENT                       { $1 }
;

/* Optional name of superinterface for interfaces */

opt_superinterface:
    /*empty*/
        { None }
  | COLON IDENT
        { Some $2 }
;

/* Import list */

import_list:
    STRING
        { [$1] }
  | import_list COMMA STRING
        { $3 :: $1 }
;


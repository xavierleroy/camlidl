(***********************************************************************)
(*                                                                     *)
(*                              CamlIDL                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0                *)
(*                                                                     *)
(***********************************************************************)

(* $Id: parse_aux.ml,v 1.12 2001-06-29 13:30:00 xleroy Exp $ *)

(* Auxiliary functions for parsing *)

open Printf
open Cvttyp
open Idltypes
open Funct
open Typedef
open Constdecl
open Intf
open File
open Linenum

module StringSet = Set.Make(struct type t = string let compare = compare end)

let null_attr_var = Expr_string ""

let no_bounds =
  { bound = None; size = None; length = None;
    is_string = false; maybe_null = false; null_terminated = false }

let one_bound n = { no_bounds with bound = Some n }

let no_switch = { discriminant = null_attr_var }

let no_enum_attr = { bitset = false }

let default_ptrkind = Unique (* as per the MIDL specs *)
let default_intkind = Iunboxed (* backward compatibility with CamlIDL 1.0 *)
let default_longkind = Iunboxed (* backward compatibility with CamlIDL 1.0 *)

let pointer_default = ref default_ptrkind
let int_default = ref default_intkind
let long_default = ref default_longkind

(* Apply a size_is or length_is attribute to an array or pointer type *)

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
  | (_, Type_bigarray(attr, ty_elt)) ->
      let dims' = merge_bigarray_dims merge_fun rexps attr.dims in
      Type_bigarray({attr with dims = dims'}, ty_elt)
  | (_, Type_const ty') ->
      Type_const (merge_array_attr merge_fun rexps ty')
  | (_, _) ->
      eprintf "%t: Warning: size_is or length_is attribute applied to \
               type `%a', ignored.\n" print_location out_c_type ty;
      ty

and merge_bigarray_dims merge_fun rexps dims =
  match (rexps, dims) with
    ([], _) -> dims
  | (_, []) -> eprintf "%t: Warning: too many dimensions in size_is or \
                        length_is attribute, extra dimensions ignored\n"
                       print_location;
               []
  | (re::res, d::ds) ->
      merge_fun d re :: merge_bigarray_dims merge_fun res ds

(* Convert an array or pointer type to a bigarray type *)

let make_bigarray ty =
  (* Extract "spine" of array / pointer types,
     with dimensions and type of elements *)
  let rec extract_spine dims = function
    Type_pointer(kind, ty) ->
      extract_spine (no_bounds :: dims) ty
  | Type_array(attr, ty) ->
      extract_spine (attr :: dims) ty
  | Type_const((Type_pointer(_,_) | Type_array(_,_)) as ty') ->
      extract_spine dims ty'
  | ty ->
      (List.rev dims, ty) in
  let (dims, ty_tail) = extract_spine [] ty in
  match ty_tail with
    Type_int(_,_) | Type_float | Type_double 
  | Type_const(Type_int(_,_) | Type_float | Type_double) ->
      Type_bigarray({dims = dims; fortran_layout = false; malloced = false},
                    ty_tail)
  | _ ->
      eprintf "%t: Warning: bigarray attribute applied to type `%a', ignored\n"
              print_location out_c_type ty;
      ty

(* Apply a type-related attribute to a type *)

let is_star_attribute name = String.length name >= 1 && name.[0] = '*'
let star_attribute name = String.sub name 1 (String.length name - 1)

let rec apply_type_attribute ty attr =
  match (attr, ty) with
  | (("nativeint", _), Type_int((Int|UInt|Long|ULong as kind), _)) ->
      Type_int(kind, Inative)
  | (("int32", _), Type_int((Int|UInt|Long|ULong as kind), _)) ->
      Type_int(kind, I32)
  | (("int64", _), Type_int((Int|UInt|Long|ULong as kind), _)) ->
      Type_int(kind, I64)
  | (("camlint", _), Type_int((Int|UInt|Long|ULong as kind), _)) ->
      Type_int(kind, Iunboxed)
  | (("ref", _), Type_pointer(attr, ty_elt)) ->
      Type_pointer(Ref, ty_elt)
  | (("unique", _), Type_pointer(attr, ty_elt)) ->
      Type_pointer(Unique, ty_elt)
  | (("unique", _), Type_array(attr, ty_elt)) ->
      Type_array({attr with maybe_null = true}, ty_elt)
  | (("ptr", _), Type_pointer(attr, ty_elt)) ->
      Type_pointer(Ptr, ty_elt)
  | (("ignore", _), Type_pointer(attr, ty_elt)) ->
      Type_pointer(Ignore, ty_elt)
  | (("string", _), Type_array(attr, ty_elt)) ->
      Type_array({attr with is_string = true}, ty_elt)
  | (("string", _), Type_pointer(attr, ty_elt)) ->
      let attr' = {no_bounds with is_string = true;
                                  maybe_null = (attr = Unique)} in
      Type_array(attr', ty_elt)
  | (("null_terminated", _), Type_array(attr, ty_elt))->
      Type_array({attr with null_terminated = true}, ty_elt)
  | (("null_terminated", _), Type_pointer(attr, ty_elt)) ->
      Type_array({no_bounds with null_terminated = true}, ty_elt)
  | (("size_is", rexps),
     (Type_array(_, _) | Type_pointer(_, _) | Type_bigarray(_, _))) ->
      merge_array_attr (fun attr re -> {attr with size = Some re})
                       rexps ty
  | (("length_is", rexps),
     (Type_array(_, _) | Type_pointer(_, _) | Type_bigarray(_, _))) ->
      merge_array_attr (fun attr re -> {attr with length = Some re})
                       rexps ty
  | (("bigarray", _), (Type_array(_, _) | Type_pointer(_, _))) ->
      make_bigarray ty
  | (("fortran", _), Type_bigarray(attrs, ty_elt)) ->
      Type_bigarray({attrs with fortran_layout = true}, ty_elt)
  | (("managed", _), Type_bigarray(attrs, ty_elt)) ->
      Type_bigarray({attrs with malloced = true}, ty_elt)
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
  | (_, Type_const ty') ->
      Type_const(apply_type_attribute ty' attr)
  | ((name, _), _) ->
      eprintf
        "%t: Warning: attribute `%s' unknown, malformed or not \
         applicable here, ignored.\n"
        print_location name;
      ty

let apply_type_attributes = List.fold_left apply_type_attribute

let rec ref_pointer = function
    Type_pointer(_, ty_elt) ->
      Type_pointer(Ref, ty_elt)
  | Type_array(attr, ty_elt) ->
      Type_array({attr with maybe_null = false}, ty_elt)
  | Type_const ty ->
      Type_const(ref_pointer ty)
  | ty -> ty

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
      let ty' = ref_pointer ty in
      merge_attributes mode' ty' rem
  | attr :: rem ->
      merge_attributes mode (apply_type_attribute ty attr) rem in
  merge_attributes None ty attrs

let make_fun_declaration attrs ty_res name params quotes =
  let call = ref None and dealloc = ref None in
  let parse_quote (label, text) =
    match String.lowercase label with
      "call" -> call := Some text
    | "dealloc" | "free" -> dealloc := Some text
    | _ ->
        eprintf "%t: Warning: quote type `%s' unknown, ignoring the quote.\n"
                print_location label in
  List.iter parse_quote quotes;
  let truename = ref name in
  let rec merge_attributes ty = function
      [] -> ty
    | (("callback" | "local"), _) :: rem ->
          merge_attributes ty rem
    | ("propget", _) :: rem ->
          truename := "get_" ^ name; merge_attributes ty rem
    | ("propput", _) :: rem ->
          truename := "put_" ^ name; merge_attributes ty rem
    | ("propputref", _) :: rem ->
          truename := "putref_" ^ name; merge_attributes ty rem
    | attr :: rem ->
          merge_attributes (apply_type_attribute ty attr) rem in
  { fun_name = !truename;
    fun_mod = "";
    fun_res = merge_attributes ty_res attrs;
    fun_params = params;
    fun_call = !call;
    fun_dealloc = !dealloc }

let make_field attrs tybase decl =
  let rec mlname default = function
    [] -> default
  | ("mlname", [Expr_ident s]) :: rem -> mlname s rem
  | (_, _) :: rem -> mlname default rem in
  let (name, ty) = decl tybase in
  { field_name = name; field_mlname = mlname name attrs;
    field_typ = apply_type_attributes ty attrs }

let make_fields attrs tybase decls =
  List.map (make_field attrs tybase) decls

let make_discriminated_union name switch_name switch_type body =
  let ty_union =
    Type_union({ud_name = ""; ud_mod = ""; ud_stamp = 0; ud_cases = body},
               {discriminant = Expr_ident switch_name}) in
  { sd_name = name; sd_mod = ""; sd_stamp = 0;
    sd_fields = [ {field_name = switch_name; field_mlname = switch_name;
                   field_typ = switch_type};
                  {field_name = "u"; field_mlname = "u";
                   field_typ = ty_union} ] }

let type_names =
  ref (List.fold_right
        (fun itf s -> StringSet.add itf.intf_name s)
        Predef.interfaces
        (List.fold_right
          (fun td s -> StringSet.add td.td_name s)
          Predef.typedefs
          StringSet.empty))

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
  | ("mltype", [Expr_string f]) :: rem ->
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
    type_names := StringSet.add name !type_names;
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

let update_int_default dfl arg =
  match arg with
    [Expr_ident "camlint"] -> dfl := Iunboxed
  | [Expr_ident "nativeint"] -> dfl := Inative
  | [Expr_ident "int32"] -> dfl := I32
  | [Expr_ident "int64"] -> dfl := I64
  | _ -> ()

let update_defaults attrs =
  List.iter
    (function
        ("pointer_default", [Expr_ident "ref"]) -> pointer_default := Ref
      | ("pointer_default", [Expr_ident "unique"]) -> pointer_default := Unique
      | ("pointer_default", [Expr_ident "ptr"]) -> pointer_default := Ptr
      | ("int_default", arg) -> update_int_default int_default arg
      | ("long_default", arg) -> update_int_default long_default arg
      | _ -> ())
    attrs

let default_stack =
  ref ([] : (pointer_kind * integer_repr * integer_repr) list)

let save_defaults () =
  default_stack :=
    (!pointer_default, !int_default, !long_default) :: !default_stack

let restore_defaults () =
  match !default_stack with
    [] -> assert false
  | (pd, id, ld) :: rem ->
      pointer_default := pd;
      int_default := id;
      long_default := ld;
      default_stack := rem

let make_interface name attrs superintf comps =
  let obj = ref false in
  let uid = ref "" in
  let parse_attr = function
    ("object", _) -> obj := true
  | ("uuid", [Expr_string u]) -> uid := u
  | ("pointer_default", _) -> () (*treated elsewhere*)
  | ("int_default", _) -> () (*treated elsewhere*)
  | ("long_default", _) -> () (*treated elsewhere*)
  | ("local", _) -> () (*ignored*)
  | ("endpoint", _) -> () (*ignored*)
  | ("version", _) -> () (*ignored*)
  | ("implicit_handle", _) -> () (*ignored*)
  | ("auto_handle", _) -> () (*ignored*)
  | (name, _) ->
        eprintf "%t: Warning: attribute `%s' unknown, malformed or not \
                 applicable here, ignored.\n" print_location name in
  List.iter parse_attr attrs;
  let supername =
    match superintf with
      None ->
        if not !obj then "" else begin
          eprintf "%t: Warning: no super-interface for interface `%s', \
                   assuming IUnknown.\n"
                  print_location name;
          "IUnknown"
        end
    | Some s ->
        if !obj then s else begin
          eprintf "%t: Warning: interface `%s' is not an object interface, \
                   ignoring super-interface `%s'.\n"
                  print_location name s;
          ""
        end in
  if not !obj then
    List.rev comps
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
    type_names := StringSet.add name !type_names;
    Comp_interface intf :: others @ [Comp_interface intf_forward]
  end

let make_forward_interface name =
  let rec intf =
    { intf_name = name; intf_mod = ""; intf_super = intf;
      intf_methods = []; intf_uid = "" } in
  Comp_interface intf

let make_diversion (id, txt) =
  let kind =
    match String.lowercase id with
      "" | "c" -> Div_c
    | "h" -> Div_h
    | "ml" -> Div_ml
    | "mli" -> Div_mli
    | "mlmli" -> Div_ml_mli
    | _ ->
      eprintf "%t: Warning: diversion kind `%s' unknown, assuming C kind.\n"
              print_location id;
      Div_c in
  (kind, txt)

(* Build an integer type *)

let make_int kind =
  match kind with
    Int | UInt -> Type_int(kind, !int_default)
  | Long | ULong -> Type_int(kind, !long_default)
  | Hyper | UHyper -> Type_int(kind, I64)
  | k -> Type_int(kind, Iunboxed) (* small int types always unboxed *)

(* Apply an "unsigned" or "signed" modifier to an integer type *)

let make_unsigned kind =
  make_int (match kind with
             Int -> UInt | Long -> ULong | Hyper -> UHyper 
           | Small -> USmall | Short -> UShort | Char -> UChar | SChar -> UChar
           | k -> k)

let make_signed kind =
  make_int (match kind with
             UInt -> Int | ULong -> Long | UHyper -> Hyper
           | USmall -> Small | UShort -> Short | Char -> SChar | UChar -> SChar
           | k -> k)

(* Warn about the handle_t type *)

let handle_t_type() =
  eprintf
    "%t: Warning: type `handle_t' unsupported, \
     treating as an opaque pointer.\n"
    print_location;
  Type_pointer(Ptr, Type_int(Int, Iunboxed))

(* Warn about the wchar_t type *)

let wchar_t_type() =
  eprintf "%t: Warning: type `wchar_t' unsupported, treating as `char'.\n"
          print_location;
  Type_int(Char, Iunboxed)

(* Apply a "star" modifier to an attribute *)

let make_star_attribute (name, args) = ("*" ^ name, args)

(* Add a "const" modifier to a type *)

let make_type_const ty =
  match ty with
    Type_const _ ->
      eprintf "%t: Warning: multiple `const' modifiers on a type.\n" 
              print_location;
      ty
  | _ -> Type_const ty

(* Forward declaration for Parse.read_file *)

let read_file = ref ((fun _ -> assert false) : string -> File.components)

(* Read an import if not already done *)

let imports = ref StringSet.empty

let read_import name =
  if StringSet.mem name !imports then
    []
  else begin
    imports := StringSet.add name !imports;
    [Comp_import(name, !read_file name)]
  end

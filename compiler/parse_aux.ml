(***********************************************************************)
(*                                                                     *)
(*                              CamlIDL                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(***********************************************************************)

(* $Id: parse_aux.ml,v 1.6 1999-03-15 15:21:38 xleroy Exp $ *)

(* Auxiliary functions for parsing *)

open Printf
open Cvttyp
open Idltypes
open Funct
open Typedef
open Constdecl
open Intf
open File

module StringSet = Set.Make(struct type t = string let compare = compare end)

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

let make_fun_declaration attrs ty_res name params quotes =
  let call = ref None and dealloc = ref None in
  let parse_quote (label, text) =
    match String.lowercase label with
      "call" -> call := Some text
    | "dealloc" | "free" -> dealloc := Some text
    | _ ->
        eprintf "Warning: quote type `%s' unknown, ignoring the quote.\n"
                label in
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

let update_pointer_default attrs =
  List.iter
    (function
        ("pointer_default", [Expr_ident "ref"]) -> pointer_default := Ref
      | ("pointer_default", [Expr_ident "unique"]) -> pointer_default := Unique
      | ("pointer_default", [Expr_ident "ptr"]) -> pointer_default := Ptr
      | _ -> ())
    attrs

let make_interface name attrs superintf comps =
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
    comps
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

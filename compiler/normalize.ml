(* Normalization of IDL types after parsing *)

open Printf
open Utils
open Idltypes
open Typedef
open Funct
open Constdecl
open Intf
open File

let structs = (Hashtbl.create 13 : (string, struct_decl) Hashtbl.t)
let unions =  (Hashtbl.create 13 : (string, union_decl) Hashtbl.t)
let enums =   (Hashtbl.create 13 : (string, enum_decl) Hashtbl.t)
let intfs =   (Hashtbl.create 13 : (string, interface) Hashtbl.t)
let typedefs =(Hashtbl.create 13 : (string, type_decl) Hashtbl.t)

let rec iunknown =
  { intf_name = "IUnknown"; intf_mod = "Com";
    intf_super = iunknown;
    intf_methods = [];
    intf_uid = "\000\000\000\000\000\000\000\000\192\000\000\000\000\000\000\070" }
    (* 00000000-0000-0000-C000-000000000046 *)

let all_comps = ref ([] : component list)

let currstamp = ref 0

let newstamp () = incr currstamp; !currstamp

let in_fundecl = ref false

let error_if_fundecl kind =
  if !in_fundecl then
    error (sprintf "anonymous %s in function parameters or result type" kind)

(* Generic function to handle declarations and definitions of struct,
   unions, enums and interfaces *)

let process_declarator kind tbl name sourcedecl 
                       proj_contents make_decl update_decl =
  if name = "" then begin
    if !in_fundecl then
     error (sprintf "anonymous %s in function parameters or result type" kind);
    let newdecl = make_decl() in
    update_decl newdecl sourcedecl;
    newdecl
  end else begin
    let decl =
      try
        Hashtbl.find tbl name
      with Not_found ->
        let newdecl = make_decl() in
        Hashtbl.add tbl name newdecl;
        newdecl in
    if proj_contents sourcedecl <> [] then begin
      if proj_contents decl <> [] then
        error (sprintf "redefinition of %s %s" kind name);
      update_decl decl sourcedecl
    end;
    decl
  end

(* Normalize types and declarators *)

let rec normalize_type = function
    Type_pointer(kind, ty_elt) ->
      Type_pointer(kind, normalize_type ty_elt)
  | Type_array(attr, ty_elt) ->
      Type_array(attr, normalize_type ty_elt)
  | Type_struct sd ->
      Type_struct(enter_struct sd)
  | Type_union(ud, discr) ->
      Type_union(enter_union ud, discr)
  | Type_enum (en, attr) ->
      Type_enum(enter_enum en, attr)
  | Type_named(_, s) ->
      begin try
        let itf = Hashtbl.find intfs s in
        Type_interface(itf.intf_mod, itf.intf_name)
      with Not_found ->
      try
        let td = Hashtbl.find typedefs s in
        Type_named(td.td_mod, td.td_name)
      with Not_found ->
        error("Unknown type name " ^ s)
      end
  | ty -> ty

and normalize_field f =
  {f with field_typ = normalize_type f.field_typ}

and normalize_case c =
  match c.case_field with
    None -> c
  | Some f -> {c with case_field = Some(normalize_field f)}

and enter_struct sd =
  process_declarator "struct" structs sd.sd_name sd
    (fun sd -> sd.sd_fields)
    (fun () ->
      { sd_name = sd.sd_name; sd_mod = !module_name;
        sd_stamp = 0; sd_fields = [] })
    (fun sd' sd ->
      sd'.sd_stamp <- newstamp();
      sd'.sd_fields <- List.map normalize_field sd.sd_fields;
      all_comps := Comp_structdecl sd' :: !all_comps)

and enter_union ud =
  process_declarator "union" unions ud.ud_name ud
    (fun ud -> ud.ud_cases)
    (fun () ->
      { ud_name = ud.ud_name; ud_mod = !module_name;
        ud_stamp = 0; ud_cases = [] })
    (fun ud' ud ->
      ud'.ud_stamp <- newstamp();
      ud'.ud_cases <- List.map normalize_case ud.ud_cases;
      all_comps := Comp_uniondecl ud' :: !all_comps)

and enter_enum en =
  process_declarator "enum" enums en.en_name en
    (fun en -> en.en_consts)
    (fun () ->
      { en_name = en.en_name; en_mod = !module_name;
        en_stamp = 0; en_consts = [] })
    (fun en' en ->
      en'.en_stamp <- newstamp();
      en'.en_consts <- en.en_consts;
      all_comps := Comp_enumdecl en' :: !all_comps)

let normalize_fundecl fd =
  current_function := fd.fun_name;
  in_fundecl := true;
  let res =
    { fd with
      fun_mod = !module_name;
      fun_res = normalize_type fd.fun_res;
      fun_params =
        List.map (fun (n, io, ty) -> (n,io, normalize_type ty)) fd.fun_params }
  in
  in_fundecl := false;
  current_function := "";
  res
  
let enter_typedecl td =
  let td' =
    { td with td_mod = !module_name;
              td_type = if td.td_abstract
                        then td.td_type
                        else normalize_type td.td_type } in
  all_comps := Comp_typedecl td' :: !all_comps;
  Hashtbl.add typedefs td'.td_name td'

let enter_interface i =
  process_declarator "interface" intfs i.intf_name i
    (fun i -> i.intf_methods)
    (fun () ->
      { intf_name = i.intf_name; intf_mod = !module_name;
        intf_super = i.intf_super; intf_methods = []; intf_uid = "" })
    (fun i' i ->
      let super =
        try
          Hashtbl.find intfs i.intf_super.intf_name
        with Not_found ->
          error (sprintf "unknown interface %s as super-interface of %s"
                         i.intf_super.intf_name i.intf_name) in
      i'.intf_uid <- i.intf_uid;
      i'.intf_super <- super;
      i'.intf_methods <- List.map normalize_fundecl i.intf_methods;
      all_comps := Comp_interface i' :: !all_comps)

let normalize_component = function
    Comp_typedecl td -> enter_typedecl td
  | Comp_structdecl sd -> ignore(enter_struct sd)
  | Comp_uniondecl ud -> ignore(enter_union ud)
  | Comp_enumdecl en -> ignore(enter_enum en)
  | Comp_fundecl fd ->
      all_comps := Comp_fundecl(normalize_fundecl fd) :: !all_comps
  | Comp_constdecl cd ->
      all_comps := Comp_constdecl cd :: !all_comps
  | Comp_diversion(ty, s) ->
      all_comps := Comp_diversion(ty, s) :: !all_comps
  | Comp_interface intf -> ignore(enter_interface intf)


(* Read and normalize a file, recursively processing the imports *)

module StringSet = Set.Make(struct type t = string let compare = compare end)

let imports_read = ref StringSet.empty

let rec normalize_file name =
  imports_read := StringSet.add name !imports_read;
  let filename =
    try
      find_in_path !Clflags.search_path name
    with Not_found ->
      eprintf "Cannot find file %s\n" name;
      raise Error in
  let pref =
    if Filename.check_suffix name ".idl"
    then Filename.chop_suffix name ".idl"
    else name in
  module_name := Filename.basename pref;
  let (imports, comps) = Parse.read_file filename in
  (* Recursively process the imports *)
  let importlist =
    List.map
      (fun name ->
        if StringSet.mem name !imports_read then [] else begin
          let (comps, imports) = normalize_file name in
          imports @ comps
        end)
      imports in
  (* Normalize the components and collect all type definitions *)
  module_name := Filename.basename pref;
  all_comps := [];
  currstamp := 0;
  List.iter normalize_component comps;
  let comps' = List.rev !all_comps in
  all_comps := [];
  (comps', List.concat importlist)

let process_file f =
  imports_read := StringSet.empty;
  Hashtbl.clear structs;
  Hashtbl.clear unions;
  Hashtbl.clear enums;
  Hashtbl.clear intfs;
  Hashtbl.clear typedefs;
  Hashtbl.add intfs "IUnknown" iunknown;
  let res = normalize_file f in
  imports_read := StringSet.empty;
  Hashtbl.clear structs;
  Hashtbl.clear unions;
  Hashtbl.clear enums;
  Hashtbl.clear intfs;
  res

let _ =
  Typedef.find :=
    (fun s ->
      try
        Hashtbl.find typedefs s
      with Not_found ->
        error("unknown type name " ^ s))

(* Normalization of IDL types after parsing *)

open Printf
open Utils
open Idltypes

let structs = (Hashtbl.create 13 : (string, struct_decl) Hashtbl.t)
let unions =  (Hashtbl.create 13 : (string, union_decl) Hashtbl.t)
let enums =   (Hashtbl.create 13 : (string, enum_decl) Hashtbl.t)

let all_type_decls = ref ([] : interface_component list)

let currstamp = ref 0

let newstamp () = incr currstamp; !currstamp

let in_fundecl = ref false

let error_if_fundecl kind =
  if !in_fundecl then
    error (sprintf "anonymous %s in function parameters or result type" kind)

let rec normalize_type = function
    Type_pointer(kind, ty_elt) ->
      Type_pointer(kind, normalize_type ty_elt)
  | Type_array(attr, ty_elt) ->
      Type_array(attr, normalize_type ty_elt)
  | Type_struct {sd_fields = []; sd_name = name} ->
      begin try
        Type_struct(Hashtbl.find structs name)
      with Not_found ->
        error (sprintf "Unknown struct %s in type" name)
      end
  | Type_struct sd ->
      Type_struct(enter_struct sd)
  | Type_union({ud_cases = []; ud_name = name}, discr) ->
      begin try
        Type_union(Hashtbl.find unions name, discr)
      with Not_found ->
        error (sprintf "Unknown union %s in type" name)
      end
  | Type_union(ud, discr) ->
      Type_union(enter_union ud, discr)
  | Type_enum {en_consts = []; en_name = name} ->
      begin try
        Type_enum(Hashtbl.find enums name)
      with Not_found ->
        error (sprintf "Unknown enum %s in type" name)
      end
  | Type_enum en ->
      Type_enum(enter_enum en)
  | ty -> ty

and normalize_field f =
  {f with field_typ = normalize_type f.field_typ}

and normalize_case c =
  match c.case_field with
    None -> c
  | Some f -> {c with case_field = Some(normalize_field f)}

and enter_struct sd =
  if sd.sd_fields = [] then begin
    let sd' = { sd_name = sd.sd_name; sd_stamp = 0; sd_fields = [] } in
    Hashtbl.add structs sd.sd_name sd';
    sd
  end else begin
    let sd' =
      try
        Hashtbl.find structs sd.sd_name
      with Not_found ->
        let sd' = { sd_name = sd.sd_name; sd_stamp = 0; sd_fields = [] } in
        if sd.sd_name <> "" then Hashtbl.add structs sd.sd_name sd';
        sd' in
    sd'.sd_stamp <- newstamp();
    sd'.sd_fields <- List.map normalize_field sd.sd_fields;
    all_type_decls := Comp_structdecl sd' :: !all_type_decls;
    sd'
  end

and enter_union ud =
  if ud.ud_cases = [] then begin
    let ud' = { ud_name = ud.ud_name; ud_stamp = 0; ud_cases = [] } in
    Hashtbl.add unions ud.ud_name ud';
    ud
  end else begin
    let ud' =
      try
        Hashtbl.find unions ud.ud_name
      with Not_found ->
        let ud' = { ud_name = ud.ud_name; ud_stamp = 0; ud_cases = [] } in
        if ud.ud_name <> "" then Hashtbl.add unions ud.ud_name ud';
        ud' in
    ud'.ud_stamp <- newstamp();
    ud'.ud_cases <- List.map normalize_case ud.ud_cases;
    all_type_decls := Comp_uniondecl ud' :: !all_type_decls;
    ud'
  end

and enter_enum en =
  if en.en_consts = [] then begin
    let en' = { en_name = en.en_name; en_stamp = 0; en_consts = [] } in
    Hashtbl.add enums en.en_name en';
    en
  end else begin
    let en' =
      try
        Hashtbl.find enums en.en_name
      with Not_found ->
        let en' = { en_name = en.en_name; en_stamp = 0; en_consts = [] } in
        if en.en_name <> "" then Hashtbl.add enums en.en_name en';
        en' in
    en'.en_stamp <- newstamp();
    en'.en_consts <- en.en_consts;
    all_type_decls := Comp_enumdecl en' :: !all_type_decls;
    en'
  end

let normalize_fundecl fd =
  current_function := fd.fun_name;
  in_fundecl := true;
  let res =
    { fd with 
      fun_res = normalize_type fd.fun_res;
      fun_params =
        List.map (fun (n, io, ty) -> (n,io, normalize_type ty)) fd.fun_params }
  in
  in_fundecl := false;
  current_function := "";
  res
  
let enter_typedecl td =
  let td' = { td with td_type = normalize_type td.td_type } in
  all_type_decls := Comp_typedecl td' :: !all_type_decls;
  td'

let normalize_component = function
    Comp_typedecl td -> Comp_typedecl(enter_typedecl td)
  | Comp_structdecl sd -> Comp_structdecl(enter_struct sd)
  | Comp_uniondecl ud -> Comp_uniondecl(enter_union ud)
  | Comp_enumdecl en -> Comp_enumdecl(enter_enum en)
  | Comp_fundecl fd -> Comp_fundecl(normalize_fundecl fd)
  | Comp_diversion s -> Comp_diversion s

let interface intf =
  let intf' = List.map normalize_component intf in
  let alldecls = !all_type_decls in
  Hashtbl.clear structs;
  Hashtbl.clear unions;
  Hashtbl.clear enums;
  all_type_decls := [];
  (intf', alldecls)

  

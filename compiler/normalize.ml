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
  let sd' =
    { sd_name = sd.sd_name;
      sd_stamp = newstamp();
      sd_fields = List.map normalize_field sd.sd_fields } in
  if sd.sd_name <> "" then Hashtbl.add structs sd.sd_name sd';
  all_type_decls := Comp_structdecl sd' :: !all_type_decls;
  sd'

and enter_union ud =
  let ud' =
    { ud_name = ud.ud_name;
      ud_stamp = newstamp();
      ud_cases = List.map normalize_case ud.ud_cases } in
  if ud.ud_name <> "" then Hashtbl.add unions ud.ud_name ud';
  all_type_decls := Comp_uniondecl ud' :: !all_type_decls;
  ud'

and enter_enum en =
  let en' =
    { en_name = en.en_name;
      en_stamp = newstamp();
      en_consts = en.en_consts } in
  if en.en_name <> "" then Hashtbl.add enums en.en_name en';
  all_type_decls := Comp_enumdecl en' :: !all_type_decls;
  en'

let normalize_fundecl fd =
  { fun_name = fd.fun_name;
    fun_res = normalize_type fd.fun_res;
    fun_params =
      List.map (fun (n, io, ty) -> (n,io, normalize_type ty)) fd.fun_params }

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

let interface intf =
  let intf' = List.map normalize_component intf in
  let alldecls = !all_type_decls in
  Hashtbl.clear structs;
  Hashtbl.clear unions;
  Hashtbl.clear enums;
  all_type_decls := [];
  (intf', alldecls)

  

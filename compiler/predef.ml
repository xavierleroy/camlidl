(* Predefined types and interfaces *)

open Idltypes
open Typedef
open Intf

let hresult =
  { td_name = "HRESULT"; td_mod = "Com";
    td_type = Type_int Long;
    td_abstract = false;
    td_c2ml = None; td_ml2c = None;
    td_errorcode = true;
    td_errorcheck = Some "camlidl_check_hresult";
    td_mltype = None }

let hresult_bool =
  { td_name = "HRESULT_bool"; td_mod = "Com";
    td_type = Type_int Long;
    td_abstract = false;
    td_c2ml = Some "camlidl_c2ml_hresult_bool";
    td_ml2c = Some "camlidl_ml2c_hresult_bool";
    td_errorcode = false;
    td_errorcheck = Some "camlidl_check_hresult";
    td_mltype = Some "bool" }

let hresult_int =
  { td_name = "HRESULT_int"; td_mod = "Com";
    td_type = Type_int Long;
    td_abstract = false;
    td_c2ml = Some "camlidl_c2ml_hresult_int";
    td_ml2c = Some "camlidl_ml2c_hresult_int";
    td_errorcode = false;
    td_errorcheck = Some "camlidl_check_hresult";
    td_mltype = Some "int" }

let rec iunknown =
  { intf_name = "IUnknown"; intf_mod = "Com";
    intf_super = iunknown;
    intf_methods = [];
    intf_uid = "\000\000\000\000\000\000\000\000\
                \192\000\000\000\000\000\000\070" }
    (* 00000000-0000-0000-C000-000000000046 *)

let typedefs = [hresult; hresult_bool; hresult_int]
let interfaces = [iunknown]

open Printf
open Utils
open Idltypes
open Variables
open Cvttyp

(* Translate the ML value [v] and store it into the C lvalue [c].
   [ty] is the IDL type of the value being converted.
   [pref] is the access prefix for the dependent parameters (size,
   discriminants, etc) to be updated. *)

let rec ml_to_c oc pref ty v c =
  match ty with
    Type_int(Long | ULong) ->
      iprintf oc "%s = Long_val(%s);\n" c v
  | Type_int _ ->
      iprintf oc "%s = Int_val(%s);\n" c v
  | Type_float | Type_double ->
      iprintf oc "%s = Double_val(%s);\n" c v
  | Type_void ->
      ()
  | Type_struct sd ->
      if sd.sd_name = "" then
        Struct.struct_ml_to_c ml_to_c oc sd v c
      else begin
        iprintf oc "camlidl_ml2c_%s_struct_%s(%s, &%s);\n"
                   !module_name sd.sd_name v c;
        need_deallocation := true
      end
  | Type_union(ud, attr) ->
      if ud.ud_name = "" then
        Union.union_ml_to_c ml_to_c oc ud v c
                            (pref ^ string_of_restr_expr attr.discriminant)
      else begin
        iprintf oc "%s%a = camlidl_ml2c_%s_union_%s(%s, &%s);\n"
                   pref out_restr_expr attr.discriminant
                   !module_name ud.ud_name v c;
        need_deallocation := true
      end
  | Type_enum(en, attr) ->
      if attr.bitset then
        Enum.enumset_ml_to_c ml_to_c oc en v c
      else if en.en_name = "" then
        Enum.enum_ml_to_c ml_to_c oc en v c
      else
        iprintf oc "%s = camlidl_ml2c_%s_enum_%s(%s);\n"
                   c !module_name en.en_name v
  | Type_named s ->
      iprintf oc "camlidl_ml2c_%s_%s(%s, &%s);\n" !module_name s v c;
      need_deallocation := true
  | Type_pointer(kind, ty_elt) ->
      begin match kind with
        Ref ->
          let c' = new_c_variable ty_elt in
          ml_to_c oc pref ty_elt v c';
          iprintf oc "%s = &%s;\n" c c'
      | Unique ->
          let v' = new_ml_variable() in
          let c' = new_c_variable ty_elt in
          iprintf oc "if (%s == Val_int(0)) {\n" v;
          increase_indent();
          iprintf oc "%s = NULL;\n" c;
          decrease_indent();
          iprintf oc "} else {\n";
          increase_indent();
          iprintf oc "%s = Field(%s, 0);\n" v' v;
          ml_to_c oc pref ty_elt v' c';
          iprintf oc "%s = &%s;\n" c c';
          decrease_indent();
          iprintf oc "}\n"
      | Ptr ->
          iprintf oc "%s = (%a) Field(%s, 0);\n" c out_c_type ty v
      | Ignore ->
          iprintf oc "%s = NULL;\n" c
      end
  | Type_array(attr, ty_elt) ->
      Array.array_ml_to_c ml_to_c oc pref attr ty_elt v c

(* Translate the C value [c] and store it into the ML variable [v].
   [ty] is the IDL type of the value being converted.
   [pref] is the access prefix for the dependent parameters (size,
   discriminants, etc) to be updated. *)

let rec c_to_ml oc pref ty c v =
  match ty with
    Type_int(Long | ULong) ->
      iprintf oc "%s = Val_long(%s);\n" v c
  | Type_int _ ->
      iprintf oc "%s = Val_int(%s);\n" v c
  | Type_float | Type_double ->
      iprintf oc "%s = copy_double(%s);\n" v c
  | Type_void ->
      ()
  | Type_struct sd ->
      if sd.sd_name = ""
      then Struct.struct_c_to_ml c_to_ml oc sd c v
      else iprintf oc "%s = camlidl_c2ml_%s_struct_%s(&%s);\n"
                      v !module_name sd.sd_name c
  | Type_union(ud, attr) ->
      if ud.ud_name = ""
      then Union.union_c_to_ml c_to_ml oc ud c v
                               (pref ^ string_of_restr_expr attr.discriminant)
      else iprintf oc "%s = camlidl_c2ml_%s_union_%s(%s%a, &%s);\n"
                      v !module_name ud.ud_name pref
                      out_restr_expr attr.discriminant c
  | Type_enum(en, attr) ->
      if attr.bitset then
        Enum.enumset_c_to_ml c_to_ml oc en c v
      else if en.en_name = "" then
        Enum.enum_c_to_ml c_to_ml oc en c v
      else
        iprintf oc "%s = camlidl_c2ml_%s_enum_%s(%s);\n"
                   v !module_name en.en_name c
  | Type_named s ->
      iprintf oc "%s = camlidl_c2ml_%s_%s(&%s);\n" v !module_name s c
  | Type_pointer(kind, ty_elt) ->
      begin match kind with
        Ref ->
          c_to_ml oc pref ty_elt (sprintf "*%s" c) v;
      | Unique ->
          iprintf oc "if (%s == NULL) {\n" c;
          increase_indent();
          iprintf oc "%s = Val_int(0);\n" v;
          decrease_indent();
          iprintf oc "} else {\n";
          increase_indent();
          let v' = new_ml_variable() in
          c_to_ml oc pref ty_elt (sprintf "*%s" c) v';
          iprintf oc "Begin_root(%s)\n" v';
          increase_indent();
          iprintf oc "%s = camlidl_alloc_small(1, 0);\n" v;
          iprintf oc "Field(%s, 0) = %s;\n" v v';
          decrease_indent();
          iprintf oc "End_roots();\n";
          decrease_indent();
          iprintf oc "}\n"
      | Ptr ->
          iprintf oc "%s = camlidl_alloc_small(1, Abstract_tag);\n" v;
          iprintf oc "Field(%s, 0) = (value) %s;\n" v c
      | Ignore ->
          ()
      end
  | Type_array(attr, ty_elt) ->
      Array.array_c_to_ml c_to_ml oc pref attr ty_elt c v

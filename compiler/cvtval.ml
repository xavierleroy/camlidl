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

(* $Id: cvtval.ml,v 1.20 2000-08-18 11:23:03 xleroy Exp $ *)

open Printf
open Utils
open Idltypes
open Variables
open Cvttyp

(* Allocate space to hold a C value of type [ty], and store a pointer to
   this space in [c].
   If [on_stack] is true, the space is allocated on stack.
   Otherwise, it is allocated in the heap. *)

let allocate_space oc onstack ty c =
  if onstack then begin
    let c' = new_c_variable ty in
    iprintf oc "%s = &%s;\n" c c';
    c'
  end else begin
    iprintf oc "%s = (%a *) camlidl_malloc(sizeof(%a), _ctx);\n"
            c out_c_type ty out_c_type ty;
    "*" ^ c
  end

(* Helper functions to deal with option types / NULL pointers *)

let option_ml_to_c oc v c conv =
  iprintf oc "if (%s == Val_int(0)) {\n" v;
  increase_indent();
  iprintf oc "%s = NULL;\n" c;
  decrease_indent();
  iprintf oc "} else {\n";
  increase_indent();
  let v' = new_ml_variable() in
  iprintf oc "%s = Field(%s, 0);\n" v' v;
  conv v';
  decrease_indent();
  iprintf oc "}\n"

let option_c_to_ml oc c v conv =
  iprintf oc "if (%s == NULL) {\n" c;
  increase_indent();
  iprintf oc "%s = Val_int(0);\n" v;
  decrease_indent();
  iprintf oc "} else {\n";
  increase_indent();
  let v' = new_ml_variable() in
  conv v';
  iprintf oc "Begin_root(%s)\n" v';
  increase_indent();
  iprintf oc "%s = camlidl_alloc_small(1, 0);\n" v;
  iprintf oc "Field(%s, 0) = %s;\n" v v';
  decrease_indent();
  iprintf oc "End_roots();\n";
  decrease_indent();
  iprintf oc "}\n"

(* Translate the ML value [v] and store it into the C lvalue [c].
   [ty] is the IDL type of the value being converted.
   [pref] is the access prefix for the dependent parameters (size,
   discriminants, etc) to be updated.
   [onstack] is true if C structures should be allocated on stack
   (their lifetime is that of the current function).
   [onstack] is false if C structures should be heap-allocated
   (they may be returned by the current function). *)

let rec ml_to_c oc onstack pref ty v c =
  match ty with
    Type_int(kind, repr) ->
      let conv =
        match repr with
          Iunboxed ->
            if kind = Long || kind = ULong then "Long_val" else "Int_val"
        | Inative -> "Nativeint_val"
        | I32 -> "Int32_val"
        | I64 -> "Int64_val" in
      iprintf oc "%s = %s(%s);\n" c conv v
  | Type_float | Type_double ->
      iprintf oc "%s = Double_val(%s);\n" c v
  | Type_void ->
      ()
  | Type_struct sd ->
      if sd.sd_name = "" then
        Struct.struct_ml_to_c ml_to_c oc onstack sd v c
      else begin
        iprintf oc "camlidl_ml2c_%s_struct_%s(%s, &%s, _ctx);\n"
                   sd.sd_mod sd.sd_name v c;
        need_context := true
      end
  | Type_union(ud, attr) ->
      if ud.ud_name = "" then
        Union.union_ml_to_c ml_to_c oc onstack ud v c
                            (Lexpr.tostring pref attr.discriminant)
      else begin
        iprintf oc "%a = camlidl_ml2c_%s_union_%s(%s, &%s, _ctx);\n"
                   Lexpr.output (pref, attr.discriminant)
                   ud.ud_mod ud.ud_name v c;
        need_context := true
      end
  | Type_enum(en, attr) ->
      if attr.bitset then
        Enum.enumset_ml_to_c ml_to_c oc en v c
      else if en.en_name = "" then
        Enum.enum_ml_to_c ml_to_c oc en v c
      else
        iprintf oc "%s = camlidl_ml2c_%s_enum_%s(%s);\n"
                   c en.en_mod en.en_name v
  | Type_named(modl, name) ->
      iprintf oc "camlidl_ml2c_%s_%s(%s, &%s, _ctx);\n" modl name v c;
      need_context := true
  | Type_pointer(Ref, Type_interface(modl, name)) ->
      iprintf oc "%s = (struct %s *) camlidl_unpack_interface(%s, _ctx);\n"
                 c name v;
      need_context := true
  | Type_pointer(Ref, ty_elt) ->
      let c' = allocate_space oc onstack ty_elt c in
      ml_to_c oc onstack pref ty_elt v c'
  | Type_pointer(Unique, ty_elt) ->
      option_ml_to_c oc v c
        (fun v' -> ml_to_c oc onstack pref (Type_pointer(Ref, ty_elt)) v' c)
  | Type_pointer(Ptr, ty_elt) ->
      iprintf oc "%s = (%a) Field(%s, 0);\n" c out_c_type ty v
  | Type_pointer(Ignore, ty_elt) ->
      iprintf oc "%s = NULL;\n" c
  | Type_array({maybe_null=false} as attr, ty_elt) ->
      Array.array_ml_to_c ml_to_c oc onstack pref attr ty_elt v c
  | Type_array({maybe_null=true} as attr, ty_elt) ->
      option_ml_to_c oc v c
        (fun v' ->
          Array.array_ml_to_c ml_to_c oc onstack pref attr ty_elt v' c)
  | Type_bigarray(attr, ty_elt) ->
      Array.bigarray_ml_to_c oc pref attr ty_elt v c
  | Type_interface(modl, name) ->
      error (sprintf "Reference to interface %s that is not a pointer" name)

(* Translate the C value [c] and store it into the ML variable [v].
   [ty] is the IDL type of the value being converted.
   [pref] is the access prefix for the dependent parameters (size,
   discriminants, etc) to be updated. *)

let rec c_to_ml oc pref ty c v =
  match ty with
    Type_int(kind, repr) ->
      let conv =
        match repr with
          Iunboxed ->
            if kind = Long || kind = ULong then "Val_long" else "Val_int"
        | Inative -> "copy_nativeint"
        | I32 -> "copy_int32"
        | I64 -> "copy_int64" in
      iprintf oc "%s = %s(%s);\n" v conv c
  | Type_float | Type_double ->
      iprintf oc "%s = copy_double(%s);\n" v c
  | Type_void ->
      ()
  | Type_struct sd ->
      if sd.sd_name = ""
      then Struct.struct_c_to_ml c_to_ml oc sd c v
      else iprintf oc "%s = camlidl_c2ml_%s_struct_%s(&%s, _ctx);\n"
                      v sd.sd_mod sd.sd_name c;
      need_context := true
  | Type_union(ud, attr) ->
      if ud.ud_name = ""
      then Union.union_c_to_ml c_to_ml oc ud c v
                               (Lexpr.tostring pref attr.discriminant)
      else iprintf oc "%s = camlidl_c2ml_%s_union_%s(%a, &%s, _ctx);\n"
                      v ud.ud_mod ud.ud_name
                      Lexpr.output (pref, attr.discriminant) c;
      need_context := true
  | Type_enum(en, attr) ->
      if attr.bitset then
        Enum.enumset_c_to_ml c_to_ml oc en c v
      else if en.en_name = "" then
        Enum.enum_c_to_ml c_to_ml oc en c v
      else
        iprintf oc "%s = camlidl_c2ml_%s_enum_%s(%s);\n"
                   v en.en_mod en.en_name c
  | Type_named(modl, name) ->
      iprintf oc "%s = camlidl_c2ml_%s_%s(&%s, _ctx);\n" v modl name c;
      need_context := true
  | Type_pointer(Ref, Type_interface(modl, name)) ->
      iprintf oc "%s = camlidl_pack_interface(%s, _ctx);\n" v c;
      need_context := true
  | Type_pointer(Ref, ty_elt) ->
      c_to_ml oc pref ty_elt (sprintf "*%s" c) v;
  | Type_pointer(Unique, ty_elt) ->
      option_c_to_ml oc c v
        (c_to_ml oc pref (Type_pointer(Ref, ty_elt)) c)
  | Type_pointer(Ptr, ty_elt) ->
      iprintf oc "%s = camlidl_alloc_small(1, Abstract_tag);\n" v;
      iprintf oc "Field(%s, 0) = (value) %s;\n" v c
  | Type_pointer(Ignore, ty_elt) ->
      ()
  | Type_array({maybe_null=false} as attr, ty_elt) ->
      Array.array_c_to_ml c_to_ml oc pref attr ty_elt c v
  | Type_array({maybe_null=true} as attr, ty_elt) ->
      option_c_to_ml oc c v
        (Array.array_c_to_ml c_to_ml oc pref attr ty_elt c)
  | Type_bigarray(attr, ty_elt) ->
      Array.bigarray_c_to_ml oc pref attr ty_elt c v
  | Type_interface(modl, name) ->
      error (sprintf "Reference to interface %s that is not a pointer" name)

(* Allocate suitable space for the C out parameter [c]. *)

let allocate_output_space oc c ty =
  match ty with
    Type_pointer(attr, ty_arg) ->
      let c' = new_c_variable ty_arg in
      iprintf oc "%s = &%s;\n" c c'
  | Type_array(attr, ty_arg) ->
      Array.array_allocate_output_space oc attr ty_arg c
  | Type_bigarray(attr, ty_arg) ->
      Array.bigarray_allocate_output_space oc attr ty_arg c
  | _ -> ()

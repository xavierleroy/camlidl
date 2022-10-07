(***********************************************************************)
(*                                                                     *)
(*                              CamlIDL                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Lesser General Public License LGPL v2.1 *)
(*                                                                     *)
(***********************************************************************)

(* $Id: array.ml,v 1.17 2002-01-16 09:42:00 xleroy Exp $ *)

(* Handling of arrays and bigarrays *)

open Printf
open Utils
open Variables
open Idltypes
open Cvttyp

(* Recognize float IDL types *)

let is_float_type =
  function Type_float -> true | Type_double -> true | _ -> false

(* Recognize IDL types whose conversion C -> ML performs no allocation.
   Due to the special treatment of float arrays, float and double
   are also treated as "no allocation". *)

let rec no_allocation_type = function
    Type_int(_, Iunboxed) -> true
  | Type_float -> true
  | Type_double -> true
  | Type_pointer(kind, ty) -> kind = Ref && no_allocation_type ty
  | Type_enum _ -> true
  | Type_const ty -> no_allocation_type ty
  | _ -> false  

(* Update dependent size variables *)

let update_size_variable svar oc pref size =
  match svar with
    None -> ()
  | Some re when Lexpr.is_identifier_deref re ->
    iprintf oc "%a = %s;\n" Lexpr.output (pref, re) size
  | Some re ->
    error "Array size expression too complex for ML -> C conversion"

(* Translation from an ML array [v] to a C array [c] *)

let array_ml_to_c ml_to_c oc onstack pref attr ty_elt v c =
  if attr.is_string || attr.is_bytes then begin
    match attr.bound with
    | None ->
        if onstack then
          iprintf oc "%s = (%a) String_val(%s);\n"
                  c
                  out_c_type (Type_pointer(Ptr, ty_elt))
                  v
        else begin
          iprintf oc "%s = camlidl_malloc_string(%s, _ctx);\n" c v;
          need_context := true
        end;
        begin match attr.size with
          None -> ()
        | Some re -> iprintf oc "%a = caml_string_length(%s);\n" 
                             Lexpr.output (pref, re) v
        end
    | Some n ->
        let size = new_c_variable (Type_named("", "mlsize_t")) in
        iprintf oc "%s = caml_string_length(%s);\n" size v;    
        iprintf oc
            "if (%s >= %d) caml_invalid_argument(\"%s\");\n"
            size (Lexpr.eval_int n) !current_function;
        iprintf oc "memcpy(%s, String_val(%s), %s + 1);\n" c v size;
        begin match attr.size with
          None -> ()
        | Some re -> iprintf oc "%a = %s;\n" 
                             Lexpr.output (pref, re) size
        end
  end else begin
    (* Determine actual size of ML array *)
    let size = new_c_variable (Type_named("", "mlsize_t")) in
    if is_float_type ty_elt
    then iprintf oc "%s = Wosize_val(%s) / Double_wosize;\n" size v
    else iprintf oc "%s = Wosize_val(%s);\n" size v;
    begin match attr.bound with
      None ->
        (* Allocate C array of same size as ML array *)
        iprintf oc "%s = camlidl_malloc(" c;
        if attr.null_terminated
        then fprintf oc "(%s + 1)" size
        else fprintf oc "%s" size;
        fprintf oc " * sizeof(%a), _ctx);\n" out_c_type ty_elt;
        need_context := true;
    | Some n ->
        (* Check compatibility of actual size w.r.t. expected size *)
        iprintf oc "if (%s %s %d) caml_invalid_argument(\"%s\");\n"
                (if attr.null_terminated then size ^ " + 1" else size)
                (if attr.size = None && not attr.null_terminated
                 then "!=" else ">")
                (Lexpr.eval_int n) !current_function
    end;
    (* Copy the array elements *)
    let idx = new_c_variable (Type_named("", "mlsize_t")) in
    begin match attr with
      {bound = Some n; size = None} ->
        iprintf oc "for (%s = 0; %s < %d; %s++) {\n"
                   idx idx (Lexpr.eval_int n) idx
    | _ ->
        iprintf oc "for (%s = 0; %s < %s; %s++) {\n"
                   idx idx size idx
    end;
    increase_indent();
    if is_float_type ty_elt then
      iprintf oc "%s[%s] = Double_field(%s, %s);\n" c idx v idx
    else begin
      let v' = new_ml_variable() in
      iprintf oc "%s = Field(%s, %s);\n" v' v idx;
      ml_to_c oc onstack pref ty_elt v' (sprintf "%s[%s]" c idx)
    end;
    decrease_indent();
    iprintf oc "}\n";
    (* Null-terminate the array if requested *)
    if attr.null_terminated then iprintf oc "%s[%s] = 0;\n" c size;
    update_size_variable attr.size oc pref size;
    update_size_variable attr.length oc pref size
  end

(* Translation from a C array [c] to an ML array [v] *)

let array_c_to_ml c_to_ml oc pref attr ty_elt c v =
  if attr.is_string || attr.is_bytes then
    iprintf oc "%s = caml_copy_string(%s);\n" v c
  else begin
    (* Determine size of ML array *)
    let (nsize, size) =
      match attr with
        {length = Some re} ->
          (max_int, Lexpr.tostring pref re)
      | {size = Some re} ->
          (max_int, Lexpr.tostring pref re)
      | {bound = Some le} ->
          let n = Lexpr.eval_int le in
          (n, string_of_int n)
      | {null_terminated = true} ->
          let sz = new_c_variable (Type_named("", "mlsize_t")) in
          iprintf oc "%s = camlidl_ptrarray_size((void **) %s);\n" sz c;
          (max_int, sz)
      | _ ->
          error "Cannot determine array size for C -> ML conversion" in
    (* Allocate ML array *)
    let alloc_function =
      if nsize < 64 && no_allocation_type ty_elt
      then "camlidl_alloc_small" else "camlidl_alloc" in
    if is_float_type ty_elt
    then iprintf oc "%s = %s(%s * Double_wosize, Double_array_tag);\n"
                  v alloc_function size
    else iprintf oc "%s = %s(%s, 0);\n" v alloc_function size;
    if not (no_allocation_type ty_elt) then begin
      iprintf oc "Begin_root(%s)\n" v;
      increase_indent()
    end;
    (* Copy elements of C array *)
    let idx = new_c_variable (Type_named("", "mlsize_t")) in
    iprintf oc "for (%s = 0; %s < %s; %s++) {\n" idx idx size idx;
    increase_indent();
    if is_float_type ty_elt then
      iprintf oc "Store_double_field(%s, %s, %s[%s]);\n" v idx c idx
    else if nsize < 64 && no_allocation_type ty_elt then
      c_to_ml oc pref ty_elt (sprintf "%s[%s]" c idx)
                             (sprintf "Field(%s, %s)" v idx)
    else begin
      let v' = new_ml_variable() in
      c_to_ml oc pref ty_elt (sprintf "%s[%s]" c idx) v';
      iprintf oc "caml_modify(&Field(%s, %s), %s);\n" v idx v'
    end;
    decrease_indent();
    iprintf oc "}\n";
    (* Pop root if needed *)
    if not (no_allocation_type ty_elt) then begin
      decrease_indent();    
      iprintf oc "End_roots()\n"
    end
  end

(* Determine the output size of an array *)

let array_output_size attr =
  match attr with
    {length = Some re} -> re
  | {size = Some re} -> re
  | {bound = Some le} -> le
  | _ -> error "Cannot determine array size for C -> ML conversion"

(* Allocate room for an out array *)

let array_allocate_output_space oc pref attr ty_elt c =
  if attr.bound = None then begin
    iprintf oc "%s = camlidl_malloc(%a * sizeof(%a), _ctx);\n"
               c Lexpr.output (pref, array_output_size attr)
               out_c_type ty_elt;
    need_context := true
  end

(* Translation from an ML bigarray [v] to a C array [c] *)

let bigarray_ml_to_c oc pref attr ty_elt v c =
  iprintf oc "%s = Caml_ba_data_val(%s);\n" c v;
  (* Update dependent size variables, if any *)
  iter_index
    (fun i attr ->
      match attr.size with
        None -> ()
      | Some re -> iprintf oc "%a = Caml_ba_array_val(%s)->dim[%d];\n" 
                           Lexpr.output (pref, re) v i)
    0 attr.dims

(* Return the flags to alloc_bigarray_dims corresponding to the given
   big array attributes *)

let bigarray_alloc_kind = function
    Type_int((Char | UChar | Byte), _) -> "CAML_BA_UINT8"
  | Type_int((SChar | Small), _) -> "CAML_BA_SINT8"
  | Type_int(Short, _) -> "CAML_BA_SINT16"
  | Type_int(UShort, _) -> "CAML_BA_UINT16"
  | Type_int((Int | UInt), _) -> "CAML_BA_INT32"
  | Type_int((Long | ULong), I64) -> "CAML_BA_INT64"
  | Type_int((Long | ULong), _) -> "CAML_BA_NATIVE_INT"
  | Type_int((Hyper | UHyper), _) -> "CAML_BA_INT64"
  | Type_float -> "CAML_BA_FLOAT32"
  | Type_double -> "CAML_BA_FLOAT64"
  | _ -> assert false

let bigarray_alloc_layout attr =
  if attr.fortran_layout
  then "CAML_BA_FORTRAN_LAYOUT"
  else "CAML_BA_C_LAYOUT"

let bigarray_alloc_managed attr =
  if attr.malloced
  then "CAML_BA_MANAGED"
  else "CAML_BA_EXTERNAL"

(* Translation from a C array [c] to an ML bigarray [v] *)

let bigarray_c_to_ml oc pref attr ty_elt c v =
  iprintf oc "%s = caml_ba_alloc_dims(\n" v;
  iprintf oc "        %s | %s | %s,\n"
             (bigarray_alloc_kind ty_elt)
             (bigarray_alloc_layout attr)
             (bigarray_alloc_managed attr);
  iprintf oc "        %d, %s" (List.length attr.dims) c;
  List.iter
    (fun attr ->
      let e = array_output_size attr in
      fprintf oc ", %a" Lexpr.output (pref, e))
    attr.dims;
  fprintf oc ");\n"

(* Allocate room for an out bigarray *)

let bigarray_allocate_output_space oc pref attr ty_elt c =
  (* Since the conversion to ML bigarray does not copy the data,
     we must allocate permanent space using stat_alloc
     (instead of transient space using camlidl_alloc),
     and we set the "malloced" attribute to true so that the
     ML bigarray will be managed by the Caml GC *)
  iprintf oc "%s = caml_stat_alloc(" c;
  List.iter
    (fun a -> fprintf oc "%a * " Lexpr.output (pref, array_output_size a))
    attr.dims;
  fprintf oc "sizeof(%a));\n" out_c_type ty_elt;
  attr.malloced <- true

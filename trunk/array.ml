(* Handling of arrays *)

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
    Type_int _ -> true
  | Type_float -> true
  | Type_double -> true
  | Type_pointer(kind, ty) -> kind = Ref && no_allocation_type ty
  | Type_enum _ -> true
  | _ -> false  

(* Translation from an ML array [v] to a C array [c] *)

let array_ml_to_c ml_to_c oc pref attr ty_elt v c =
  if attr.is_string then begin
    begin match attr.bound with
      None ->
        iprintf oc "%s = String_val(%s);\n" c v
    | Some n ->
        iprintf oc
            "if (string_length(%s) >= %d) invalid_argument(\"%s\");\n"
            v n !current_function;
        iprintf oc "strcpy(%s, String_val(%s));\n" c v
    end;
    begin match attr.size with
      None -> ()
    | Some re -> iprintf oc "%s%a = string_length(%s);\n" 
                         pref out_restr_expr re v
    end
  end else begin
    (* Determine actual size of ML array *)
    let size = new_c_variable (Type_named "mlsize_t") in
    if is_float_type ty_elt
    then iprintf oc "%s = Wosize_val(%s) / Double_wosize;\n" size v
    else iprintf oc "%s = Wosize_val(%s);\n" size v;
    begin match attr.bound with
      None ->
        (* Allocate C array of same size as ML array *)
        iprintf oc "%s = camlidl_temp_alloc(" c;
        if attr.null_terminated
        then fprintf oc "(%s + 1)" size
        else fprintf oc "%s" size;
        fprintf oc " * sizeof(%a));\n" out_c_type ty_elt;
        need_deallocation := true;
    | Some n ->
        (* Check compatibility of actual size w.r.t. expected size *)
        iprintf oc "if (%s %s %d) invalid_argument(\"%s\");\n"
                (if attr.null_terminated then size ^ " + 1" else size)
                (if attr.size = None && not attr.null_terminated
                 then "!=" else ">")
                n !current_function
    end;
    (* Copy the array elements *)
    let idx = new_c_variable (Type_named "mlsize_t") in
    begin match attr with
      {bound = Some n; size = None} ->
        iprintf oc "for (%s = 0; %s < %d; %s++) {\n" idx idx n idx
    | _ ->
        iprintf oc "for (%s = 0; %s < %s; %s++) {\n" idx idx size idx
    end;
    increase_indent();
    if is_float_type ty_elt then
      iprintf oc "%s[%s] = Double_field(%s, %s);\n" c idx v idx
    else begin
      let v' = new_ml_variable() in
      iprintf oc "%s = Field(%s, %s);\n" v' v idx;
      ml_to_c oc pref ty_elt v' (sprintf "%s[%s]" c idx)
    end;
    decrease_indent();
    iprintf oc "}\n";
    (* Null-terminate the array if requested *)
    if attr.null_terminated then iprintf oc "%s[%s] = 0;\n" c size;
    (* Update dependent size variable *)
    begin match attr.size with
      None -> ()
    | Some re -> iprintf oc "%s%a = %s;\n" pref out_restr_expr re size
    end
  end

(* Translation from a C array [c] to an ML array [v] *)

let array_c_to_ml c_to_ml oc pref attr ty_elt c v =
  if attr.is_string then
    iprintf oc "%s = copy_string(%s);\n" v c
  else begin
    (* Determine size of ML array *)
    let (nsize, size) =
      match attr with
        {length = Some re} ->
          (max_int, pref ^ string_of_restr_expr re)
      | {size = Some re} ->
          (max_int, pref ^ string_of_restr_expr re)
      | {bound = Some n} ->
          (n, string_of_int n)
      | {null_terminated = true} ->
          let sz = new_c_variable (Type_named "mlsize_t") in
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
    let idx = new_c_variable (Type_named "mlsize_t") in
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
      iprintf oc "modify(&Field(%s, %s), %s);\n" v idx v'
    end;
    decrease_indent();
    iprintf oc "}\n";
    (* Pop root if needed *)
    if not (no_allocation_type ty_elt) then begin
      decrease_indent();    
      iprintf oc "End_roots()\n"
    end
  end

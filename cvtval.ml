open Printf
open Utils
open Idltypes
open Variables
open Cvttyp

(* Output a reference to a restricted expression *)

let string_of_restr_expr = function Var s -> s | Deref s -> "*" ^ s

let out_restr_expr oc e = output_string oc (string_of_restr_expr e)

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
  | Type_pointer(attr, ty) -> attr.ptrkind = Ref && no_allocation_type ty
  | Type_enum _ -> true
  | _ -> false  

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
  | Type_struct s ->
      iprintf oc "_camlidl_ml2c_%s_struct_%s(%s, &%s);\n" !module_name s v c
  | Type_union(s, discr) ->
      iprintf oc "%s%a = _camlidl_ml2c_%s_union_%s(%s, &%s);\n"
              pref out_restr_expr discr !module_name s v c
  | Type_enum s ->
      iprintf oc "_camlidl_ml2c_%s_enum_%s(%s, &%s);\n"
              !module_name s v c
  | Type_named s ->
      iprintf oc "_camlidl_ml2c_%s_%s(%s, &%s);\n" !module_name s v c
  | Type_pointer(attr, ty_elt) ->
      begin match attr.ptrkind with
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
      end
  | Type_array(attr, ty_elt) ->
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
            iprintf oc "%s = (%a) stat_alloc(%s * sizeof(%a));\n"
                    c out_c_type ty size out_c_type ty_elt;
            add_to_deallocate c
        | Some n ->
            (* Check compatibility of actual size w.r.t. expected size *)
            iprintf oc "if (%s %s %d) invalid_argument(\"%s\");\n"
                    size (if attr.size = None then "!=" else ">")
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
        (* Update dependent size variable *)
        begin match attr.size with
          None -> ()
        | Some re -> iprintf oc "%s%a = %s;\n" pref out_restr_expr re size
        end
      end

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
  | Type_struct s ->
      iprintf oc "%s = _camlidl_c2ml_%s_struct_%s(&%s);\n" v !module_name s c
  | Type_union(s, discr) ->
      iprintf oc "%s = _camlidl_c2ml_%s_union_%s(%s%a, &%s);\n"
              v !module_name s pref out_restr_expr discr c
  | Type_enum s ->
      iprintf oc "%s = _camlidl_c2ml_%s_enum_%s(&%s);\n"
              v !module_name s c
  | Type_named s ->
      iprintf oc "%s = _camlidl_c2ml_%s_%s(&%s);\n" v !module_name s c
  | Type_pointer(attr, ty_elt) ->
      begin match attr.ptrkind with
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
          iprintf oc "%s = alloc_small(1, 0);\n" v;
          iprintf oc "Field(%s, 0) = %s;\n" v v';
          decrease_indent();
          iprintf oc "End_roots();\n";
          decrease_indent();
          iprintf oc "}\n"
      | Ptr ->
          iprintf oc "%s = alloc(1, Abstract_tag);\n" v;
          iprintf oc "Field(%s, 0) = (value) %s;\n" v c
      end
  | Type_array(attr, ty_elt) ->
      if attr.is_string then
        iprintf oc "%s = copy_string(%s);\n" v c
      else begin
        (* Determine size of ML array *)
        let (nsize, size) =
          match attr with
            {length = Some re} -> (max_int, pref ^ string_of_restr_expr re)
          | {size = Some re} -> (max_int, pref ^ string_of_restr_expr re)
          | {bound = Some n} -> (n, string_of_int n)
          | _ -> error "Cannot determine array size for C -> ML conversion" in
        (* Allocate ML array *)
        let alloc_function =
          if nsize < 64 && no_allocation_type ty_elt
          then "alloc_small" else "alloc" in
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



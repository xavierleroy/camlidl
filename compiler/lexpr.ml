(* Evaluation and pretty-printing of limited expressions *)

open Printf
open Idltypes
open Utils

type constant_value = Cst_int of int | Cst_string of string

(* Bind values to constant names *)

let const_val = (Hashtbl.create 29 : (string, constant_value) Hashtbl.t)

let bind_const name v =
  Hashtbl.add const_val name v

(* Evaluate a limited expression to an integer *)

let rec eval_int = function
    Expr_ident v ->
      begin try
        match Hashtbl.find const_val v with
          Cst_int n -> n
        | Cst_string s ->
            error (sprintf "String constant %s used in integer context" v)
      with Not_found ->
        error (sprintf "%s is not a constant" v)
      end
  | Expr_int n -> n
  | Expr_string s ->
      error (sprintf "String literal \"%s\" used in integer context"
                     (String.escaped s))
  | Expr_cond (e1, e2, e3) ->
      if eval_int e1 <> 0 then eval_int e2 else eval_int e3
  | Expr_sequand (e1, e2) ->
      let v1 = eval_int e1 in if v1 <> 0 then eval_int e2 else v1
  | Expr_sequor (e1, e2) ->
      let v1 = eval_int e1 in if v1 = 0 then eval_int e2 else v1
  | Expr_logor (e1, e2) ->
      eval_int e1 lor eval_int e2
  | Expr_logxor (e1, e2) ->
      eval_int e1 lxor eval_int e2
  | Expr_logand (e1, e2) ->
      eval_int e1 land eval_int e2
  | Expr_eq (e1, e2) ->
      if eval_int e1 = eval_int e2 then 1 else 0
  | Expr_ne (e1, e2) ->
      if eval_int e1 <> eval_int e2 then 1 else 0
  | Expr_lt (e1, e2) ->
      if eval_int e1 < eval_int e2 then 1 else 0
  | Expr_gt (e1, e2) ->
      if eval_int e1 > eval_int e2 then 1 else 0
  | Expr_le (e1, e2) ->
      if eval_int e1 <= eval_int e2 then 1 else 0
  | Expr_ge (e1, e2) ->
      if eval_int e1 >= eval_int e2 then 1 else 0
  | Expr_lshift (e1, e2) ->
      eval_int e1 lsl eval_int e2
  | Expr_rshift (e1, e2) ->
      eval_int e1 asr eval_int e2
  | Expr_plus (e1, e2) ->
      eval_int e1 + eval_int e2
  | Expr_minus (e1, e2) ->
      eval_int e1 - eval_int e2
  | Expr_times (e1, e2) ->
      eval_int e1 * eval_int e2
  | Expr_div (e1, e2) ->
      let v1 = eval_int e1 and v2 = eval_int e2 in
      if v2 = 0 then error "division by zero";
      v1 / v2
  | Expr_mod (e1, e2) ->
      let v1 = eval_int e1 and v2 = eval_int e2 in
      if v2 = 0 then error "modulo by zero";
      v1 mod v2
  | Expr_neg e1 ->
      - (eval_int e1)
  | Expr_lognot e1 ->
      lnot(eval_int e1)
  | Expr_boolnot e1 ->
      if eval_int e1 = 0 then 1 else 0
  | Expr_cast(ty, e1) ->
      eval_int e1
  (* TO DO: sizeof? *)
  | _ ->
      error("non-integer operator used in an integer context")

let eval = function
    Expr_ident v ->
      begin try
        Hashtbl.find const_val v
      with Not_found ->
        error (sprintf "%s is not a constant" v)
      end
  | Expr_string s ->
      Cst_string s
  | e ->
      Cst_int(eval_int e)

(* Pretty-print a limited expression *)

open Ebuff

let b = create 80

let rec tstype trail = function
    Type_int kind -> add_string b (integer_type kind); add_string b trail
  | Type_float -> add_string b "float"; add_string b trail
  | Type_double -> add_string b "double"; add_string b trail
  | Type_void -> add_string b "void"; add_string b trail
  | Type_struct sd ->
      assert (sd.sd_name <> "");
      add_string b "struct "; add_string b sd.sd_name; add_string b trail
  | Type_union(ud, discr) ->
      assert (ud.ud_name <> "");
      add_string b "union "; add_string b ud.ud_name; add_string b trail
  | Type_enum (en, attr) ->
      add_string b "int"; add_string b trail
  | Type_named(modl, ty_name) ->
      add_string b ty_name; add_string b trail
  | Type_pointer(attr, (Type_array(_, _) as ty)) ->
      tstype (sprintf "(*%s)" trail) ty
  | Type_pointer(attr, ty) ->
      tstype (sprintf "*%s" trail) ty
  | Type_array(attr, ty) ->
      let trail' =
        match attr.bound with
          Some n -> sprintf "%s[]" trail
        | None -> sprintf "*%s" trail in
      tstype trail ty
  | Type_interface(modl, intf_name) ->
      add_string b "interface "; add_string b intf_name; add_string b trail

and integer_type = function
    Int -> "int"
  | Long -> "long"
  | Small -> "signed char"
  | Short -> "short"
  | Char -> "char"
  | UInt -> "unsigned int"
  | ULong -> "unsigned long"
  | USmall -> "unsigned char"
  | UShort -> "unsigned short"
  | UChar -> "unsigned char"
  | SChar -> "signed char"
  | Byte -> "unsigned char"
  | Boolean -> "int"

let add_escaped_string s =
  add_char b '"';
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    if c >= ' ' && c <= '~'
    then add_char b c
    else add_string b (sprintf "\\%03o" (Char.code c))
  done;
  add_char b '"'

let tostr pref e =

  let rec ts = function
      Expr_cond(e1, e2, e3) ->
        ts1 e1; add_string b " ? "; ts1 e2; add_string b " : ";
        ts1 e3
    | e -> ts1 e

  and ts1 = function
      Expr_sequor(e1, e2) -> ts2 e1; add_string b " || "; ts2 e2
    | Expr_sequand(e1, e2) -> ts2 e1; add_string b " && "; ts2 e2
    | e -> ts2 e

  and ts2 = function
      Expr_logand(e1, e2) -> ts3 e1; add_string b " & "; ts3 e2
    | Expr_logor(e1, e2) -> ts3 e1; add_string b " | "; ts3 e2
    | Expr_logxor(e1, e2) -> ts3 e1; add_string b " ^ "; ts3 e2
    | e -> ts3 e

  and ts3 = function
      Expr_eq(e1, e2) -> ts4 e1; add_string b " == "; ts4 e2
    | Expr_ne(e1, e2) -> ts4 e1; add_string b " != "; ts4 e2
    | Expr_lt(e1, e2) -> ts4 e1; add_string b " < "; ts4 e2
    | Expr_gt(e1, e2) -> ts4 e1; add_string b " > "; ts4 e2
    | Expr_le(e1, e2) -> ts4 e1; add_string b " <= "; ts4 e2
    | Expr_ge(e1, e2) -> ts4 e1; add_string b " >= "; ts4 e2
    | e -> ts4 e

  and ts4 = function
      Expr_lshift(e1, e2) -> ts5 e1; add_string b " << "; ts5 e2
    | Expr_rshift(e1, e2) -> ts5 e1; add_string b " >> "; ts5 e2
    | e -> ts5 e

  and ts5 = function
      Expr_plus(e1, e2) -> ts5 e1; add_string b " + "; ts5 e1
    | Expr_minus(e1, e2) -> ts5 e1; add_string b " + "; ts6 e1
    | e -> ts6 e

  and ts6 = function
      Expr_times(e1, e2) -> ts6 e1; add_string b " * "; ts6 e2
    | Expr_div(e1, e2) -> ts6 e1; add_string b " / "; ts7 e2
    | Expr_mod(e1, e2) -> ts6 e1; add_string b " % "; ts7 e2
    | e -> ts7 e

  and ts7 = function
      Expr_neg e -> add_string b "-"; ts7 e
    | Expr_lognot e -> add_string b "~"; ts7 e
    | Expr_boolnot e -> add_string b "!"; ts7 e
    | Expr_deref e -> add_string b "*"; ts7 e
    | Expr_addressof e -> add_string b "&"; ts7 e
    | Expr_cast(ty, e) ->
        add_string b "("; tstype "" ty; add_string b ") "; ts7 e
    | Expr_sizeof(ty) ->
        add_string b "sizeof("; tstype "" ty; add_string b ")"
    | e -> ts8 e

  and ts8 = function
      Expr_subscript(e1, e2) ->
        ts8 e1; add_string b "["; ts e2; add_string b "]"
    | Expr_dereffield(e1, s) ->
        ts8 e1; add_string b "->"; add_string b s
    | Expr_field(e1, s) ->
        ts8 e1; add_string b "."; add_string b s
    | e -> ts9 e

  and ts9 = function
      Expr_ident s ->
        begin try
          match Hashtbl.find const_val s with
            Cst_int n -> add_string b (string_of_int n)
          | Cst_string s -> add_escaped_string s
        with Not_found ->
          add_string b pref; add_string b s
        end
    | Expr_int n ->
        add_string b (string_of_int n)
    | Expr_string s ->
        add_escaped_string s
    | e ->
        add_char b '('; ts e; add_char b ')'

  in ts e

let tostring pref e =
  Ebuff.reset b; tostr pref e; Ebuff.get_stored b

let output oc (pref, e) =
  Ebuff.reset b; tostr pref e; Ebuff.output oc b

(* Check if a variable is free in a limited expression *)

let is_free v e =
  let rec free = function
    Expr_ident s -> s = v
  | Expr_int n -> false
  | Expr_string s -> false
  | Expr_cond (e1, e2, e3) -> free e1 || free e2 || free e3
  | Expr_sequand (e1, e2) -> free e1 || free e2
  | Expr_sequor (e1, e2) -> free e1 || free e2
  | Expr_logor (e1, e2) -> free e1 || free e2
  | Expr_logxor (e1, e2) -> free e1 || free e2
  | Expr_logand (e1, e2) -> free e1 || free e2
  | Expr_eq (e1, e2) -> free e1 || free e2
  | Expr_ne (e1, e2) -> free e1 || free e2
  | Expr_lt (e1, e2) -> free e1 || free e2
  | Expr_gt (e1, e2) -> free e1 || free e2
  | Expr_le (e1, e2) -> free e1 || free e2
  | Expr_ge (e1, e2) -> free e1 || free e2
  | Expr_lshift (e1, e2) -> free e1 || free e2
  | Expr_rshift (e1, e2) -> free e1 || free e2
  | Expr_plus (e1, e2) -> free e1 || free e2
  | Expr_minus (e1, e2) -> free e1 || free e2
  | Expr_times (e1, e2) -> free e1 || free e2
  | Expr_div (e1, e2) -> free e1 || free e2
  | Expr_mod (e1, e2) -> free e1 || free e2
  | Expr_neg (e1) -> free e1
  | Expr_lognot (e1) -> free e1
  | Expr_boolnot (e1) -> free e1
  | Expr_deref (e1) -> free e1
  | Expr_addressof (e1) -> free e1
  | Expr_cast (ty, e1) -> free e1
  | Expr_sizeof ty -> false
  | Expr_subscript (e1, e2) -> free e1 || free e2
  | Expr_dereffield (e1, s) -> free e1
  | Expr_field (e1, s) -> free e1
  in free e

let is_free_opt v opte =
  match opte with
    None -> false
  | Some e -> is_free v e

(* Check if a variable appears in a dependent type *)

let rec is_dependent v ty =
  match ty with
    Type_array(attr, ty) ->
      is_free_opt v attr.size ||
      is_free_opt v attr.length ||
      is_dependent v ty
  | Type_union(name, attr) ->
      is_free v attr.discriminant
  | Type_pointer(_, Type_union(name, attr)) ->
      is_free v attr.discriminant
  | Type_pointer(_, ty) ->
      is_dependent v ty
  | _ -> false


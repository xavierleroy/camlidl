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

(* $Id: lexpr.ml,v 1.11 2002-01-16 16:15:32 xleroy Exp $ *)

(* Evaluation and pretty-printing of limited expressions *)

open Printf
open Idltypes
open Utils

type constant_value =
    Cst_int of int32
  | Cst_long of nativeint
  | Cst_longlong of int64
  | Cst_string of string

(* Bind values to constant names *)

let const_val = (Hashtbl.create 29 : (string, constant_value) Hashtbl.t)

let bind_const name v =
  Hashtbl.add const_val name v

(* Evaluate a limited expression to a constant *)

let is_true = function
    Cst_int n -> n <> Int32.zero
  | Cst_long n -> n <> Nativeint.zero
  | Cst_longlong n -> n <> Int64.zero
  | Cst_string s -> true (*hmph*)

let cst_true = Cst_int Int32.one
let cst_false = Cst_int Int32.zero

let compare rel (v1, v2) =
  match (v1, v2) with
    (Cst_int n1, Cst_int n2) ->
      if rel v1 v2 then cst_true else cst_false
  | (Cst_long n1, Cst_long n2) ->
      if rel v1 v2 then cst_true else cst_false
  | (Cst_longlong n1, Cst_longlong n2) ->
      if rel v1 v2 then cst_true else cst_false
  | (_, _) ->
      error("illegal comparison")

let int_val = function
    Cst_int n -> Int32.to_int n
  | Cst_long n -> Nativeint.to_int n
  | Cst_longlong n -> Int64.to_int n
  | _ -> error "string value in integer context"

let int32_val = function
    Cst_int n -> n
  | Cst_long n -> Nativeint.to_int32 n
  | Cst_longlong n -> Int64.to_int32 n
  | _ -> error "string value in integer context"

let nativeint_val = function
    Cst_int n -> Nativeint.of_int32 n
  | Cst_long n -> n
  | Cst_longlong n -> Int64.to_nativeint n
  | _ -> error "string value in integer context"

let int64_val = function
    Cst_int n -> Int64.of_int32 n
  | Cst_long n -> Int64.of_nativeint n
  | Cst_longlong n -> n
  | _ -> error "string value in integer context"

let string_val = function
    Cst_string s -> s
  | _ -> error "integer value in string context"

(* Expand a typedef name, returning its definition *)
let expand_typedef = ref ((fun _ -> assert false) : string -> idltype)

let rec cast_value ty v =
  match ty with
    Type_int(kind, _) ->
      begin match kind with
        Int | UInt -> Cst_int(int32_val v)
      | Long | ULong -> Cst_long(nativeint_val v)
      | Hyper | UHyper -> Cst_longlong(int64_val v)
      | USmall | Char | UChar | Byte | Boolean ->
          Cst_int(Int32.logand (int32_val v) (Int32.of_int 0xFF))
      | Small | SChar ->
          Cst_int(Int32.shift_right (Int32.shift_left (int32_val v) 24) 24)
      | Short ->
          Cst_int(Int32.shift_right (Int32.shift_left (int32_val v) 16) 16)
      | UShort ->
          Cst_int(Int32.logand (int32_val v) (Int32.of_int 0xFFFF))
      end
  | Type_pointer(_, Type_int((Char|UChar|SChar), _)) |
    Type_array({is_string = true}, _) ->
      Cst_string(string_val v)
  | Type_named(modname, tyname) ->
      let ty' =
        try !expand_typedef tyname
      with Not_found ->
        error (sprintf "unknown type name %s" tyname) in
      cast_value ty' v
  | Type_const ty' ->
      cast_value ty' v
  | _ ->
      error "unsupported type for constant expression"

(* Evaluate a limited expression *)

let rec eval = function
    Expr_ident v ->
      (try Hashtbl.find const_val v
       with Not_found -> error (sprintf "%s is not a constant" v))
  | Expr_int n ->
      if n < Int64.of_int32 Int32.max_int
      && n >= Int64.of_int32 Int32.min_int
      then Cst_int(Int64.to_int32 n)
      else if n < Int64.of_nativeint Nativeint.max_int
      && n >= Int64.of_nativeint Nativeint.min_int
      then Cst_long(Int64.to_nativeint n)
      else Cst_longlong n
  | Expr_string s -> Cst_string s
  | Expr_cond (e1, e2, e3) ->
      if is_true(eval e1) then eval e2 else eval e3
  | Expr_sequand (e1, e2) ->
      let v1 = eval e1 in if is_true v1 then eval e2 else v1
  | Expr_sequor (e1, e2) ->
      let v1 = eval e1 in if is_true v1 then v1 else eval e2
  | Expr_logor (e1, e2) ->
      eval_binary Int32.logor Nativeint.logor Int64.logor e1 e2
  | Expr_logxor (e1, e2) ->
      eval_binary Int32.logxor Nativeint.logxor Int64.logxor e1 e2
  | Expr_logand (e1, e2) ->
      eval_binary Int32.logand Nativeint.logand Int64.logand e1 e2
  | Expr_eq (e1, e2) ->
      compare (=) (eval_promote e1 e2)
  | Expr_ne (e1, e2) ->
      compare (<>) (eval_promote e1 e2)
  | Expr_lt (e1, e2) ->
      compare (<) (eval_promote e1 e2)
  | Expr_gt (e1, e2) ->
      compare (>) (eval_promote e1 e2)
  | Expr_le (e1, e2) ->
      compare (<=) (eval_promote e1 e2)
  | Expr_ge (e1, e2) ->
      compare (>=) (eval_promote e1 e2)
  | Expr_lshift (e1, e2) ->
      eval_shift Int32.shift_left Nativeint.shift_left Int64.shift_left e1 e2
  | Expr_rshift (e1, e2) ->
      eval_shift Int32.shift_right Nativeint.shift_right Int64.shift_right e1 e2
  | Expr_rshift_unsigned (e1, e2) ->
      eval_shift Int32.shift_right_logical Nativeint.shift_right_logical Int64.shift_right_logical e1 e2
  | Expr_plus (e1, e2) ->
      eval_binary Int32.add Nativeint.add Int64.add e1 e2
  | Expr_minus (e1, e2) ->
      eval_binary Int32.sub Nativeint.sub Int64.sub e1 e2
  | Expr_times (e1, e2) ->
      eval_binary Int32.mul Nativeint.mul Int64.mul e1 e2
  | Expr_div (e1, e2) ->
      begin try
        eval_binary Int32.div Nativeint.div Int64.div e1 e2
      with Division_by_zero ->
        error "division by zero"
      end
  | Expr_mod (e1, e2) ->
      begin try
        eval_binary Int32.rem Nativeint.rem Int64.rem e1 e2
      with Division_by_zero ->
        error "division by zero"
      end
  | Expr_neg e1 ->
      eval_unary Int32.neg Nativeint.neg Int64.neg e1
  | Expr_lognot e1 ->
      eval_unary Int32.lognot Nativeint.lognot Int64.lognot e1
  | Expr_boolnot e1 ->
      if is_true(eval e1) then cst_false else cst_true
  | Expr_cast(ty, e1) ->
      cast_value ty (eval e1)
  | Expr_sizeof ty ->
      Cst_int(Int32.of_int(match ty with
        Type_int((Int|UInt), _) -> 4
      | Type_int((Long|ULong), _) -> Sys.word_size / 4
      | Type_int((Hyper|UHyper), _) -> 8
      | Type_int((Small|USmall|Char|UChar|SChar|Byte|Boolean), _) -> 1
      | Type_int((Short|UShort), _) -> 2
      | Type_float -> 4
      | Type_double -> 8
      | Type_pointer(_, _) -> Sys.word_size / 4
      | _ -> error "cannot compute sizeof"))
  | _ ->
      error("illegal operation in constant expression")

and eval_promote e1 e2 =
  let v1 = eval e1 and v2 = eval e2 in
  match (v1, v2) with
  | (Cst_int n1, Cst_long n2) -> (Cst_long (Nativeint.of_int32 n1), v2)
  | (Cst_long n1, Cst_int n2) -> (v1, Cst_long(Nativeint.of_int32 n2))
  | (Cst_int n1, Cst_longlong n2) -> (Cst_longlong(Int64.of_int32 n1), v2)
  | (Cst_longlong n1, Cst_int n2) -> (v1, Cst_longlong(Int64.of_int32 n2))
  | (Cst_long n1, Cst_longlong n2) -> (Cst_longlong(Int64.of_nativeint n1), v2)
  | (Cst_longlong n1, Cst_long n2) -> (v1, Cst_longlong(Int64.of_nativeint n2))
  | (_, _) -> (v1, v2)

and eval_binary op32 opnative op64 e1 e2 =
  match eval_promote e1 e2 with
    (Cst_int n1, Cst_int n2) -> Cst_int(op32 n1 n2)
  | (Cst_long n1, Cst_long n2) -> Cst_long(opnative n1 n2)
  | (Cst_longlong n1, Cst_longlong n2) -> Cst_longlong(op64 n1 n2)
  | (_, _) ->
      error("non-integer arguments to integer operation")

and eval_unary op32 opnative op64 e1 =
  match eval e1 with
    Cst_int n1 -> Cst_int(op32 n1)
  | Cst_long n1 -> Cst_long(opnative n1)
  | Cst_longlong n1 -> Cst_longlong(op64 n1)
  | _ ->
      error("non-integer argument to integer operation")

and eval_shift op32 opnative op64 e1 e2 =
  let n2 = int_val(eval e2) in
  match eval e1 with
    Cst_int n1 -> Cst_int(op32 n1 n2)
  | Cst_long n1 -> Cst_long(opnative n1 n2)
  | Cst_longlong n1 -> Cst_longlong(op64 n1 n2)
  | _ ->
      error("non-integer arguments to integer operation")

(* Evaluate a limited expression to an integer *)

let eval_int e = int_val(eval e)

(* Test if this expression is just an identifier *)

let is_identifier = function Expr_ident _ -> true | _ -> false

(* Pretty-print a limited expression *)

open Buffer

let b = create 80

let rec tstype trail = function
    Type_int(kind,_) -> add_string b (integer_type kind); add_string b trail
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
  | Type_pointer(attr, (Type_array(_,_) as ty)) ->
      tstype (sprintf "(*%s)" trail) ty
  | Type_pointer(attr, ty) ->
      tstype (sprintf "*%s" trail) ty
  | Type_array(attr, ty) ->
      let trail' =
        match attr.bound with
          Some n -> sprintf "%s[]" trail
        | None -> sprintf "*%s" trail in
      tstype trail' ty
  | Type_bigarray(attr, ty) ->
      tstype (sprintf "*%s" trail) ty
  | Type_interface(modl, intf_name) ->
      add_string b "struct "; add_string b intf_name; add_string b trail
  | Type_const ty ->
      tstype (sprintf " const %s" trail) ty

and integer_type = function
    Int -> "int"
  | Long -> "long"
  | Hyper -> Config.int64_type
  | Small -> "signed char"
  | Short -> "short"
  | Char -> "char"
  | UInt -> "unsigned int"
  | ULong -> "unsigned long"
  | UHyper -> Config.uint64_type
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
    | Expr_rshift_unsigned(e1, e2) -> (*revise!*)
        ts5 e1; add_string b " >> "; ts5 e2
    | e -> ts5 e

  and ts5 = function
      Expr_plus(e1, e2) -> ts5 e1; add_string b " + "; ts5 e2
    | Expr_minus(e1, e2) -> ts5 e1; add_string b " - "; ts6 e2
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
            Cst_int n ->
              add_string b (Int32.to_string n)
          | Cst_long n ->
              add_string b (Nativeint.to_string n); add_char b 'L'
          | Cst_longlong n ->
              add_string b (Int64.to_string n); add_string b "LL"
          | Cst_string s ->
              add_escaped_string s
        with Not_found ->
          add_string b (Prefix.for_ident pref s); add_string b s
        end
    | Expr_int n ->
        add_string b (Int64.to_string n)
    | Expr_string s ->
        add_escaped_string s
    | e ->
        add_char b '('; ts e; add_char b ')'

  in ts7 e

let tostring pref e =
  Buffer.clear b; tostr pref e;
  let res = Buffer.contents b in Buffer.reset b; res

let output oc (pref, e) =
  Buffer.clear b; tostr pref e; Buffer.output_buffer oc b; Buffer.reset b

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
  | Expr_rshift_unsigned (e1, e2) -> free e1 || free e2
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
  | Type_bigarray(attr, ty) ->
      List.exists
        (fun a -> is_free_opt v a.size || is_free_opt v a.length)
        attr.dims
      || is_dependent v ty
  | Type_union(name, attr) ->
      is_free v attr.discriminant
  | Type_pointer(_, ty) ->
      is_dependent v ty
  | Type_const ty ->
      is_dependent v ty
  | _ -> false

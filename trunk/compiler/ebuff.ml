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

(* $Id: ebuff.ml,v 1.2 1999-02-19 14:33:28 xleroy Exp $ *)

(* String buffers *)

type t =
  { initial: string;
    mutable buff: string;
    mutable index: int }

let create sz =
  let s = String.create sz in { initial = s; buff = s; index = 0 }

let reset b =
  b.buff <- b.initial; b.index <- 0

let resize_buffer b n =
  let newsize = ref (2 * String.length b.buff) in
  while b.index + n > !newsize do newsize := 2 * !newsize done;
  let new_buff = String.create !newsize in
  String.blit b.buff 0 new_buff 0 (String.length b.buff);
  b.buff <- new_buff

let add_char b c =
  if b.index >= String.length b.buff then resize_buffer b 1;
  String.unsafe_set b.buff b.index c;
  b.index <- b.index + 1

let add_substring b s ofs len =
  if b.index + len > String.length b.buff then resize_buffer b len;
  String.blit s ofs b.buff b.index len;
  b.index <- b.index + len

let add_string b s =
  add_substring b s 0 (String.length s)

let get_stored b =
  let s = String.sub b.buff 0 b.index in
  reset b;
  s

let output oc b =
  Pervasives.output oc b.buff 0 b.index;
  reset b

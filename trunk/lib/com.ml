(* Run-time library for COM components *)

type 'a opaque

type 'a interface

type 'a iid

exception Error of string

external prim_queryInterface: 'a interface -> 'b iid -> 'b interface =
  "camlidl_com_queryInterface"

let queryInterface intf iid =
  try
    prim_queryInterface intf iid
  with Failure s ->
    raise (Error s)

type iUnknown

let iUnknown_of (intf : 'a interface) = (Obj.magic intf : iUnknown interface)

let _ =
  Callback.register "Oo.new_method" Oo.new_method

external prim_combine: 'a interface -> 'b interface -> 'a interface =
  "camlidl_com_combine"

let combine i1 i2 =
  try
    prim_combine i1 i2
  with Failure s ->
    raise (Error s)


(***********************************************************************)
(*                                                                     *)
(*                              CamlIDL                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0                *)
(*                                                                     *)
(***********************************************************************)

(* $Id: typedef.ml,v 1.17 2002-05-01 15:23:15 xleroy Exp $ *)

(* Handling of typedefs *)

open Printf
open Utils
open Variables
open Idltypes
open Cvttyp
open Cvtval

type type_decl =
  { td_name: string;
    td_mod: string;
    td_type: idltype;
    td_abstract: bool;
    td_c2ml: string option;
    td_ml2c: string option;
    td_finalize: string option;
    td_compare: string option;
    td_hash: string option;
    td_errorcode: bool;
    td_errorcheck: string option;
    td_mltype: string option }

(* Record typedefs by name *)

let find =
  ref ((fun _ -> invalid_arg "Typedef.find") : string -> type_decl)

(* Generate the ML type definition corresponding to the typedef *)

let ml_declaration oc td =
  match td with
    {td_mltype = Some s} ->
      fprintf oc "%s = %s\n" (String.uncapitalize_ascii td.td_name) s
  | {td_abstract = true} ->
      fprintf oc "%s\n" (String.uncapitalize_ascii td.td_name)
  | _ ->
      fprintf oc "%s = %a\n"
              (String.uncapitalize_ascii td.td_name) out_ml_type td.td_type

(* Generate the C typedef corresponding to the typedef *)

let c_declaration oc td =
  fprintf oc "typedef %a;\n" out_c_decl (td.td_name, td.td_type);
  begin match td.td_ml2c with
    None -> ()
  | Some s -> fprintf oc "extern void %s(value, %s *);\n" s td.td_name
  end;
  begin match td.td_c2ml with
    None -> ()
  | Some s -> fprintf oc "extern value %s(%s *);\n" s td.td_name
  end;
  begin match td.td_finalize with
    None -> ()
  | Some s -> fprintf oc "extern void %s(%s *);\n" s td.td_name
  end;
  begin match td.td_compare with
    None -> ()
  | Some s ->
      fprintf oc "extern int %s(%s *, %s *);\n" s td.td_name td.td_name
  end;
  begin match td.td_hash with
    None -> ()
  | Some s -> fprintf oc "extern long %s(%s *);\n" s td.td_name
  end;
  fprintf oc "\n"

(* External (forward) declaration of the translation functions *)

let declare_transl oc td =
  begin match td.td_ml2c with
    Some s ->
      fprintf oc "extern void %s(value, %s *);\n"
                 s td.td_name;
      fprintf oc "#define camlidl_ml2c_%s_%s(v,c,ctx) %s(v,c)\n\n"
                 td.td_mod td.td_name s
  | None ->
      fprintf oc "extern void camlidl_ml2c_%s_%s(value, %s *, camlidl_ctx _ctx);\n"
                 td.td_mod td.td_name td.td_name
  end;
  begin match td.td_c2ml with
    Some s ->
      fprintf oc "extern value %s(%s *);\n"
                 s td.td_name;
      fprintf oc "#define camlidl_c2ml_%s_%s(c,ctx) %s(c)\n\n"
                 td.td_mod td.td_name s
  | None ->
      fprintf oc "extern value camlidl_c2ml_%s_%s(%s *, camlidl_ctx _ctx);\n"
                 td.td_mod td.td_name td.td_name
  end;
  fprintf oc "\n"

(* Translation function from the ML type to the C type *)

let is_custom_block td =
  td.td_abstract &&
  not (td.td_finalize = None && td.td_compare = None && td.td_hash = None)

let transl_ml_to_c oc td =
  current_function := sprintf "typedef %s" td.td_name;
  let v = new_var "_v" in
  let c = new_var "_c" in
  fprintf oc "void camlidl_ml2c_%s_%s(value %s, %s * %s, camlidl_ctx _ctx)\n"
             td.td_mod td.td_name v td.td_name c;
  fprintf oc "{\n";
  let pc = divert_output() in
  increase_indent();
  if td.td_abstract then
    if is_custom_block td then begin
      iprintf pc "*%s = *((%s *) Data_custom_val(%s));\n"
                 c td.td_name v
    end else begin
      iprintf pc "*%s = *((%s *) Bp_val(%s));\n" c td.td_name v
    end
  else begin
    ml_to_c pc false Prefix.empty td.td_type v (sprintf "(*%s)" c);
  end;
  decrease_indent();
  output_variable_declarations oc;
  end_diversion oc;
  fprintf oc "}\n\n";
  current_function := ""

(* Translation function from the C type to the ML type *)

let transl_c_to_ml oc td =
  begin match td.td_finalize with
    None -> ()
  | Some f ->
      fprintf oc "\
static void camlidl_finalize_%s_%s(value v)
{
  %s((%s *) Data_custom_val(v));
}
"       td.td_mod td.td_name f td.td_name
  end;
  begin match td.td_compare with
    None -> ()
  | Some f ->
      fprintf oc "\
static int camlidl_compare_%s_%s(value v1, value v2)
{
  return %s((%s *) Data_custom_val(v1), (%s *) Data_custom_val(v2));
}
"       td.td_mod td.td_name f td.td_name td.td_name
  end;
  begin match td.td_hash with
    None -> ()
  | Some f ->
      fprintf oc "\
static long camlidl_hash_%s_%s(value v)
{
  return %s((%s *) Data_custom_val(v));
}
"       td.td_mod td.td_name f td.td_name
  end;
  if is_custom_block td then begin
    fprintf oc "struct custom_operations camlidl_cops_%s_%s = {\n"
               td.td_mod td.td_name;
    fprintf oc "  NULL,\n";
    begin match td.td_finalize with
      None   -> iprintf oc "  custom_finalize_default,\n"
    | Some f -> iprintf oc "  camlidl_finalize_%s_%s,\n" td.td_mod td.td_name
    end;
    begin match td.td_compare with
      None   -> iprintf oc "  custom_compare_default,\n"
    | Some f -> iprintf oc "  camlidl_compare_%s_%s,\n" td.td_mod td.td_name
    end;
    begin match td.td_hash with
      None   -> iprintf oc "  custom_hash_default,\n"
    | Some f -> iprintf oc "  camlidl_hash_%s_%s,\n" td.td_mod td.td_name
    end;
    iprintf oc "  custom_serialize_default,\n";
    iprintf oc "  custom_deserialize_default\n";
    fprintf oc "};\n\n"
  end;
  current_function := sprintf "typedef %s" td.td_name;
  let v = new_ml_variable() in
  let c = new_var "_c" in
  fprintf oc "value camlidl_c2ml_%s_%s(%s * %s, camlidl_ctx _ctx)\n"
             td.td_mod td.td_name td.td_name c;
  fprintf oc "{\n";
  let pc = divert_output() in
  increase_indent();
  if td.td_abstract then
    if is_custom_block td then begin
      iprintf pc "%s = alloc_custom(&camlidl_cops_%s_%s, sizeof(%s), 0, 1);\n"
              v td.td_mod td.td_name td.td_name;
      iprintf pc "*((%s *) Data_custom_val(%s)) = *%s;\n"
              td.td_name v c
    end else begin
      iprintf pc "%s = camlidl_alloc((sizeof(%s) + sizeof(value) - 1) / sizeof(value), Abstract_tag);\n"
              v td.td_name;
      iprintf pc "*((%s *) Bp_val(%s)) = *%s;\n"
              td.td_name v c
    end
  else begin
    c_to_ml pc Prefix.empty td.td_type (sprintf "(*%s)" c) v
  end;
  iprintf pc "return %s;\n" v;
  decrease_indent();
  output_variable_declarations oc;
  end_diversion oc;
  fprintf oc "}\n\n";
  current_function := ""

(* Emit the translation functions *)

let emit_transl oc td =
  begin match td.td_ml2c with
    Some s ->
      fprintf oc "#define camlidl_ml2c_%s_%s(v,c,ctx) %s(v,c)\n\n"
                 td.td_mod td.td_name s
  | None ->
      transl_ml_to_c oc td
  end;
  begin match td.td_c2ml with
    Some s ->
      fprintf oc "#define camlidl_c2ml_%s_%s(c,ctx) %s(c)\n\n"
                 td.td_mod td.td_name s
  | None ->
      transl_c_to_ml oc td
  end

(* Generation of stub code for functions *)

open Printf
open Utils
open Variables
open Idltypes
open Typedef
open Cvttyp
open Cvtval

type in_out =
    In | Out | InOut

type function_decl =
  { fun_name: string;
    fun_res: idltype;
    fun_params: (string * in_out * idltype) list;
    fun_ccall: string option }

(* Remove dependent parameters (parameters that are size_is, length_is,
   or switch_is of another parameter).  Also remove ignored pointers. *)

let is_dependent_parameter name params =
  let is_param = function
      Var s -> s = name
    | Deref s -> s = name in
  let is_param_opt = function
      None -> false
    | Some re -> is_param re in
  List.exists
    (fun (name, inout, ty) ->
      match ty with
        Type_array(attr, ty) ->
          is_param_opt attr.size || is_param_opt attr.length
      | Type_union(name, attr) ->
          is_param attr.discriminant
      | Type_pointer(_, Type_union(name, attr)) ->
          is_param attr.discriminant
      | _ -> false)
    params

let is_ignored =
  function Type_pointer(Ignore, _) -> true | _ -> false

let remove_dependent_parameters params =
  list_filter
    (fun (name, _, ty) ->
      not (is_dependent_parameter name params || is_ignored ty))
    params

(* Split parameters into in parameters and out parameters.
   In/out get copied to both. *)

let rec split_in_out = function
    [] -> ([], [])
  | (name, inout, ty) :: rem ->
      let (ins, outs) = split_in_out rem in
      match inout with
        In -> ((name, ty) :: ins, outs)
      | Out -> (ins, (name, ty) :: outs)
      | InOut -> ((name, ty) :: ins, (name, ty) :: outs)

(* Determine if a typedef represents an error code *)

let rec is_errorcode = function
    Type_named s -> (Typedef.find s).td_errorcode
  | Type_pointer(kind, ty) -> is_errorcode ty
  | _ -> false

(* Convert the C view of parameters and result into the ML view:
    - remove dependent parameters
    - turn out and in/out parameters into extra results
    - remove void and errorcode return values *)

let ml_view fundecl =
  let true_params = remove_dependent_parameters fundecl.fun_params in
  let (ins, outs) = split_in_out true_params in
  (* Add return value as an out if it's not void *)
  let outs2 =
    if fundecl.fun_res = Type_void
    then outs
    else ("_res", fundecl.fun_res) :: outs in
  (* Remove out parameters that are error codes *)
  (ins, list_filter (fun (name, ty) -> not(is_errorcode ty)) outs2)

(* Generate the ML declaration for a function *)

let out_ml_types oc sep types =
  match types with
    [] -> fprintf oc "unit"
  | (_, ty1) :: tyl ->
      out_ml_type oc ty1;
      List.iter (fun (_, ty) -> fprintf oc " %s " sep; out_ml_type oc ty) tyl

let ml_declaration oc fundecl =
  let (ins, outs) = ml_view fundecl in
  fprintf oc "external %s : " (String.uncapitalize fundecl.fun_name);
  out_ml_types oc "->" ins;
  fprintf oc " -> ";
  out_ml_types oc "*" outs;
  if List.length ins <= 5
  then fprintf oc "\n\t= \"camlidl_%s_%s\"\n\n" !module_name fundecl.fun_name
  else fprintf oc "\n\t= \"camlidl_%s_%s_bytecode\" \"camlidl_%s_%s\"\n\n"
                  !module_name fundecl.fun_name !module_name fundecl.fun_name

(* Print a warm fuzzy in/out comment *)

let out_inout oc = function
    In -> fprintf oc "[in]"
  | Out -> fprintf oc "[out]"
  | InOut -> fprintf oc "[in,out]"

let output_deallocate oc =
  if !need_deallocation then begin
    iprintf oc "camlidl_temp_free();\n";
    need_deallocation := false
  end

(* Call an error checking function if needed *)

let rec call_error_check oc name ty =
  match ty with
    Type_named s ->
      begin match Typedef.find s with
        {td_errorcheck = Some fn} -> iprintf oc "%s(%s);\n" fn name
      | _ -> ()
      end
  | Type_pointer(kind, ty_elt) ->
      call_error_check oc ("*" ^ name) ty_elt
  | _ -> ()

(* Generate the wrapper for calling a C function from ML *)

let emit_wrapper oc fundecl =
  current_function := fundecl.fun_name;
  let (ins, outs) = ml_view fundecl in
  (* Emit function header *)
  fprintf oc "value camlidl_%s_%s(" !module_name fundecl.fun_name;
  begin match ins with
    [] ->
      fprintf oc "value _unit)\n"
  | (name1, ty1) :: rem ->
      fprintf oc "\n\tvalue _v_%s" name1;
      List.iter
        (fun (name, ty) -> fprintf oc ",\n\tvalue _v_%s" name)
        rem;
      fprintf oc ")\n"
  end;
  fprintf oc "{\n";
  (* Declare C local variables to hold parameters and result *)
  List.iter
    (fun (name, inout, ty) ->
      fprintf oc "  %a; /*%a*/\n" out_c_decl (name, ty) out_inout inout)
    fundecl.fun_params;
  if fundecl.fun_res <> Type_void then
    fprintf oc "  %a;\n" out_c_decl ("_res", fundecl.fun_res);
  let pc = divert_output() in
  (* Initialize dependent parameters that are pointers so that they
     point to suitable storage *)
  List.iter
    (function (name, (In|InOut), Type_pointer(attr, ty_arg))
              when is_dependent_parameter name fundecl.fun_params ->
                  let c = new_c_variable ty_arg in
                  iprintf pc "%s = &%s;\n" name c
            | _ -> ())
    fundecl.fun_params;
  (* Convert ins from ML to C *)
  List.iter
    (fun (name, ty) -> ml_to_c pc "" ty (sprintf "_v_%s" name) name)
    ins;
  (* Initialize outs that are pointers so that they point
     to suitable storage *)
  List.iter
    (function (name, Out, Type_pointer(attr, ty_arg)) ->
                  let c = new_c_variable ty_arg in
                  iprintf pc "%s = &%s;\n" name c
            | _ -> ())
    fundecl.fun_params;
  (* Generate call to C function *)
  begin match fundecl.fun_ccall with
    Some s ->
      iprintf pc "/* begin user-supplied calling sequence */\n";
      output_string pc s;
      iprintf pc "/* end user-supplied calling sequence */\n"
  | None ->
      if fundecl.fun_res = Type_void
      then iprintf pc ""
      else iprintf pc "_res = ";
      fprintf pc "%s(" fundecl.fun_name;
      begin match fundecl.fun_params with
        [] -> ()
      | (name1, _,_) :: rem ->
          fprintf pc "%s" name1;
          List.iter (fun (name, _, _) -> fprintf pc ", %s" name) rem
      end;
      fprintf pc ");\n"
  end;
  (* Call error checking functions on result and out parameters
     that need it *)
  call_error_check pc "_res" fundecl.fun_res;
  List.iter
    (fun (name, mode, ty) ->
        if mode = Out || mode = InOut then call_error_check pc name ty)
    fundecl.fun_params;
  (* Convert outs from C to ML *)
  begin match outs with
    [] ->
      output_variable_declarations oc;
      output_deallocate pc;
      iprintf pc "return Val_unit;\n"
  | [name, ty] ->
      c_to_ml pc "" ty name "_vres";
      output_variable_declarations oc;
      fprintf oc "  value _vres;\n\n";
      output_deallocate pc;
      iprintf pc "return _vres;\n";
  | _ ->
      let num_outs = List.length outs in
      iprintf pc "Begin_roots_block(_vres, %d)\n" num_outs;
      increase_indent();
      let pos = ref 0 in
      List.iter
        (fun (name, ty) ->
          c_to_ml pc "" ty name (sprintf "_vres[%d]" !pos);
          incr pos)
        outs;
      iprintf pc "_vresult = camlidl_alloc_small(%d, 0);\n" num_outs;
      copy_values_to_block pc "_vres" "_vresult" num_outs;
      decrease_indent();
      iprintf pc "End_roots()\n";
      output_deallocate pc;
      iprintf pc "return _vresult;\n";
      output_variable_declarations oc;
      fprintf oc "  value _vresult;\n";
      fprintf oc "  value _vres[%d] = { " num_outs;
      for i = 1 to num_outs do fprintf oc "0, " done;
      fprintf oc "};\n\n"
  end;
  end_diversion oc;
  fprintf oc "}\n\n";
  (* If more than 5 arguments, create an extra wrapper for the bytecode
     interface *)
  if List.length ins > 5 then begin
    fprintf oc "value camlidl_%s_%s_bytecode(value * argv, int argn)\n"
               !module_name fundecl.fun_name;
    fprintf oc "{\n";
    fprintf oc "  camlidl_%s_%s(argv[0]" !module_name fundecl.fun_name;
    for i = 1 to List.length ins - 1 do
      fprintf oc ", argv[%d]" i
    done;
    fprintf oc ");\n";
    fprintf oc "}\n\n"
  end;
  current_function := ""



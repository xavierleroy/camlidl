(* Handling of COM-style interfaces *)

open Utils
open Printf
open Variables
open Idltypes
open Cvttyp
open Cvtval
open Funct

type interface =
  { intf_name: string;                  (* Name of interface *)
    intf_mod: string;                   (* Name of defining module *)
    mutable intf_super: interface;      (* Super-interface *)
    mutable intf_methods: function_decl list;   (* Methods *)
    mutable intf_uid: string }          (* Unique interface ID *)

(* Print a method type *)

let out_method_type oc meth =
  let (ins, outs) = ml_view meth in
  begin match ins with
    [] -> ()
  | _  -> out_ml_types oc "->" ins;
          fprintf oc " -> "
  end;
  out_ml_types oc "*" outs

(* Print the ML abstract type identifying the interface *)

let ml_declaration oc intf =
  fprintf oc "%s\n" (String.uncapitalize intf.intf_name)

(* Declare the class *)

let ml_class_declaration oc intf =
  let mlintf = String.uncapitalize intf.intf_name in
  let mlsuper = String.uncapitalize intf.intf_super.intf_name in
  fprintf oc "class %s_class :\n" mlintf;
  fprintf oc "  %s Com.interface ->\n" mlintf;
  fprintf oc "    object\n";
  if intf.intf_super.intf_name <> "IUnknown"
  then fprintf oc "    inherit %s_class\n" mlsuper;
  List.iter
    (fun meth ->
      fprintf oc "      method %s: %a\n"
                 (String.uncapitalize meth.fun_name) out_method_type meth)
    intf.intf_methods;
  fprintf oc "    end\n\n";
  (* Declare the IID *)
  if intf.intf_uid <> "" then
    fprintf oc "val iid_%s : %s Com.iid\n" mlintf mlintf;
  (* Declare the conversion functions *)
  fprintf oc "val use_%s : %s Com.interface -> %s_class\n"
             mlintf mlintf mlintf;
  fprintf oc "val make_%s : #%s_class -> %s Com.interface\n"
             mlintf mlintf mlintf;
  fprintf oc "val %s_of_%s : %s Com.interface -> %a Com.interface\n\n"
             mlsuper mlintf mlintf
             out_mltype_name (intf.intf_super.intf_mod,
                              intf.intf_super.intf_name)

(* Declare the interface in C *)

let rec declare_vtbl oc self intf =
  if intf.intf_name = "IUnknown" then begin
    iprintf oc "DECLARE_VTBL_PADDING\n";
    iprintf oc "HRESULT (*QueryInterface)(struct %s * self, IID *, void **);\n"
               self;
    iprintf oc "ULONG (*AddRef)(struct %s * self);\n" self;
    iprintf oc "ULONG (*Release)(struct %s * self);\n" self
  end else begin
    declare_vtbl oc self intf.intf_super;
    List.iter
      (fun m ->
        iprintf oc "%a(struct %s * self"
                   out_c_decl (sprintf "(*%s)" m.fun_name, m.fun_res)
                   self;
        List.iter
          (fun (name, inout, ty) ->
            fprintf oc ", /*%a*/ %a" out_inout inout out_c_decl (name, ty))
          m.fun_params;
        fprintf oc ");\n")
      intf.intf_methods
  end

let c_declaration oc intf =
  if intf.intf_methods = [] then begin
    fprintf oc "struct %s;\n" intf.intf_name
  end else begin
    fprintf oc "struct %sVtbl {\n" intf.intf_name;
    increase_indent();
    declare_vtbl oc intf.intf_name intf;
    decrease_indent();
    fprintf oc "};\n";
    fprintf oc "struct %s {\n" intf.intf_name;
    fprintf oc "  struct %sVtbl * lpVtbl;\n" intf.intf_name;
    fprintf oc "};\n";
    fprintf oc "extern IID IID_%s;\n\n" intf.intf_name
  end

(* Define the wrapper classes *)

let ml_class_definition oc intf =
  let intfname = String.uncapitalize intf.intf_name in
  let supername = String.uncapitalize intf.intf_super.intf_name in
  (* Define the IID *)
  if intf.intf_uid <> "" then
    fprintf oc "let iid_%s = (Obj.magic \"%s\" : %s Com.iid)\n"
               intfname (String.escaped intf.intf_uid) intfname;
  (* Define the coercion function to the super class *)
  fprintf oc "let %s_of_%s (intf : %s Com.interface) = (Obj.magic intf : %a Com.interface)\n\n"
             supername intfname intfname
             out_mltype_name (intf.intf_super.intf_mod,
                              intf.intf_super.intf_name);
  (* Declare the C wrappers for invoking the methods from Caml *)
  let self_type =
    Type_pointer(Ref, Type_interface(!module_name, intf.intf_name)) in
  List.iter
    (fun meth ->
      let prim =
        { fun_name = sprintf "%s_%s" intf.intf_name meth.fun_name;
          fun_mod = intf.intf_mod;
          fun_res = meth.fun_res;
          fun_params = ("this", In, self_type) :: meth.fun_params;
          fun_call = None } in
      Funct.ml_declaration oc prim)
    intf.intf_methods;
  fprintf oc "\n";
  (* Define the wrapper class *)
  fprintf oc "class %s_class (intf : %s Com.interface) =\n" intfname intfname;
  fprintf oc "  object\n";
  if intf.intf_super.intf_name <> "IUnknown" then
    fprintf oc "    inherit (%s_class (%s_of_%s intf))\n"
               supername supername intfname;
  List.iter
    (fun meth ->
      let methname = String.uncapitalize meth.fun_name in
      fprintf oc "    method %s = %s_%s intf\n"
              methname intfname meth.fun_name)
    intf.intf_methods;
  fprintf oc "  end\n\n";
  (* Define the conversion functions *)
  fprintf oc "let use_%s = new %s_class\n" intfname intfname;
  fprintf oc "external make_%s : #%s_class -> %s Com.interface = \"camlidl_makeintf_%s_%s\"\n\n"
             intfname intfname intfname !module_name intf.intf_name

(* If context is needed, set it up (indefinite allocation, persistent
   interface refs) *)

let output_context before after =
  if !need_context then begin
    fprintf before
      "  struct camlidl_ctx _ctxs = { CAMLIDL_ADDREF, NULL };\n";
    fprintf before "  camlidl_ctx _ctx = &_ctxs;\n"
  end

(* Generate callback wrapper for calling an ML method from C *)

let emit_callback_wrapper oc intf meth =
  current_function := sprintf "%s::%s" intf.intf_name meth.fun_name;
  need_context := false;
  let (ins, outs) = ml_view meth in
  (* Emit function header *)
  let fun_name =
    sprintf "camlidl_%s_%s_%s_callback"
            !module_name intf.intf_name meth.fun_name in
  fprintf oc "%a(" out_c_decl (fun_name, meth.fun_res);
  fprintf oc "\n\tinterface %s * this" intf.intf_name;
  List.iter
    (fun (name, inout, ty) ->
    fprintf oc ",\n\t/* %a */ %a" out_inout inout out_c_decl (name, ty))
    meth.fun_params;
  fprintf oc ")\n{\n";
  (* Declare locals to hold ML arguments and result, and C result if any *)
  let num_ins = List.length ins in
  fprintf oc "  value _varg[%d] = { " (num_ins + 1);
  for i = 0 to num_ins do fprintf oc "0, " done;
  fprintf oc "};\n";
  fprintf oc "  value _vres;\n";
  fprintf oc "  static value _vlabel = 0;\n";
  if meth.fun_res <> Type_void then 
    fprintf oc "  %a;\n" out_c_decl ("_res", meth.fun_res);
  (* Convert inputs from C to Caml *)
  let pc = divert_output() in
  iprintf pc "Begin_roots_block(_varg, %d)\n" (num_ins + 1);
  increase_indent();
  iprintf pc
    "_varg[0] = ((struct camlidl_intf *) this)->caml_object;\n";
  iter_index
    (fun pos (name, ty) -> c_to_ml pc "" ty name (sprintf "_varg[%d]" pos))
    1 ins;
  (* Recover the label.
     _vlabel is not registered as a root because it's an integer. *)
  iprintf pc "if (_vlabel == 0) _vlabel = camlidl_lookup_method(\"%s\");\n"
             (String.uncapitalize meth.fun_name);
  decrease_indent();
  iprintf pc "End_roots();\n";
  (* Do the callback *)
  iprintf pc "_vres = callbackN_exn(Lookup(_varg[0], _vlabel), %d, _varg);\n"
             (num_ins + 1);
  (* Check if exception occurred *)
  iprintf pc "if (Is_exception_result(_vres))\n";
  begin match meth.fun_res with
    Type_named(_, "HRESULT") ->
      iprintf pc "  return camlidl_result_exception(\"%s.%s\", \
                             Extract_exception(_vres));\n"
                 !module_name !current_function
  | _ ->
      iprintf pc "  camlidl_uncaught_exception(\"%s\", \
                             Extract_exception(_vres));\n"
                 !current_function
  end;
  (* Convert outputs from Caml to C *)
  let convert_output ty src dst =
    match (dst, ty) with
      ("_res", _) -> ml_to_c pc false "" ty src dst
    | (_, Type_pointer(_, ty')) -> ml_to_c pc false "" ty' src ("*" ^ dst)
    | (_, _) ->
        error (sprintf "Out parameter `%s' must be a pointer" dst) in
  begin match outs with
    [] -> ()
  | [name, ty] ->
      convert_output ty "_vres" name
  | _ ->
      iter_index
        (fun pos (name, ty) ->
            convert_output ty (sprintf "Field(_vres, %d)" pos) name)
        0 outs
  end;
  output_context oc pc;
  (* Return result if any *)
  if meth.fun_res <> Type_void then
    iprintf pc "return _res;\n";
  output_variable_declarations oc;
  end_diversion oc;
  fprintf oc "}\n\n"

(* Declare external callback wrapper *)

let declare_callback_wrapper oc intf meth =
  let (ins, outs) = ml_view meth in
  (* Emit function header *)
  let fun_name =
    sprintf "camlidl_%s_%s_%s_callback"
            !module_name intf.intf_name meth.fun_name in
  fprintf oc "extern %a(" out_c_decl (fun_name, meth.fun_res);
  fprintf oc "\n\tinterface %s * this" intf.intf_name;
  List.iter
    (fun (name, inout, ty) ->
    fprintf oc ",\n\t/* %a */ %a" out_inout inout out_c_decl (name, ty))
    meth.fun_params;
  fprintf oc ");\n\n"

(* Generate the vtable for an interface (for the make_ conversion) *)

let rec emit_vtbl oc intf =
  if intf.intf_name = "IUnknown" then begin
    fprintf oc "  (void *) camlidl_QueryInterface,\n";
    fprintf oc "  (void *) camlidl_AddRef,\n";
    fprintf oc "  (void *) camlidl_Release,\n";
  end else begin
    emit_vtbl oc intf.intf_super;
    List.iter
      (fun m -> fprintf oc "  /* %s */ (void *) camlidl_%s_%s_%s_callback,\n"
                        m.fun_name !module_name intf.intf_name m.fun_name)
      intf.intf_methods
  end

let emit_vtable oc intf =
  fprintf oc "struct %sVtbl camlidl_%s_%s_vtbl = {\n"
             intf.intf_name !module_name intf.intf_name;
  fprintf oc "  VTBL_PADDING\n";
  emit_vtbl oc intf;
  fprintf oc "};\n\n"

(* Generate the make_ conversion (takes an ML object, wraps it into
   a COM interface) *)

let emit_make_interface oc intf =
  fprintf oc "value camlidl_makeintf_%s_%s(value vobj)\n"
             !module_name intf.intf_name;
  fprintf oc "{\n";
  if intf.intf_uid = "" then
    fprintf oc "  return camlidl_make_interface(&camlidl_%s_%s_vtbl, vobj, NULL);\n"
               !module_name intf.intf_name
  else
    fprintf oc "  return camlidl_make_interface(&camlidl_%s_%s_vtbl, vobj, &IID_%s);\n"
               !module_name intf.intf_name intf.intf_name;
  fprintf oc "}\n\n"

(* Definition of the translation functions *)

let emit_transl oc intf =
  List.iter (Funct.emit_method_wrapper oc intf.intf_name) intf.intf_methods;
  List.iter (emit_callback_wrapper oc intf) intf.intf_methods;
  emit_vtable oc intf;
  emit_make_interface oc intf

(* Declare the translation functions *)

let declare_transl oc intf =
  List.iter (declare_callback_wrapper oc intf) intf.intf_methods

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
    intf_super: interface option;       (* Super-interface, if any *)
    intf_methods: function_decl list;   (* Methods *)
    intf_uid: string }                  (* Unique interface ID *)

(* Build the full list of methods of an interface *)

let rec method_suite intf =
  match intf.intf_super with
    None ->
      intf.intf_methods
  | Some s ->
      method_suite s @ intf.intf_methods

(* FIXME: are we certain that the method names of the interface
   are disjoint with those of its base interface? *)

(* Print a method type *)

let out_method_type oc meth =
  let (ins, outs) = ml_view meth in
  begin match ins with
    [] -> ()
  | _  -> out_ml_types oc "->" ins;
          fprintf oc " -> "
  end;
  out_ml_types oc "*" outs

(* Print the ML object type corresponding to the interface *)

let ml_declaration oc intf =
  fprintf oc "%s = <\n" (String.uncapitalize intf.intf_name);
  List.iter
    (fun meth ->
      fprintf oc "  %s: %a;\n"
              (String.uncapitalize meth.fun_name) out_method_type meth)
    (method_suite intf);
  fprintf oc ">\n"

(* Declare the class *)

let ml_class_declaration oc intf =
  fprintf oc "class %s_class :\n" (String.uncapitalize intf.intf_name);
  fprintf oc "  #Com.iUnknown ->\n";
  fprintf oc "    object\n";
  begin match intf.intf_super with
    None ->
      fprintf oc "      val cintf: Com.interface\n"
  | Some s ->
      fprintf oc "      inherit %s_class\n" (String.uncapitalize s.intf_name)
  end;
  List.iter
    (fun meth ->
      fprintf oc "      method %s: %a\n"
                 (String.uncapitalize meth.fun_name) out_method_type meth)
    intf.intf_methods;
  fprintf oc "    end\n\n"

(* Define the wrapper classes *)

let ml_class_definition oc intf =
  let intfname = String.uncapitalize intf.intf_name in
  (* Declare the C wrappers for invoking the methods from Caml *)
  List.iter
    (fun meth ->
      let prim =
        { fun_name = sprintf "%s_%s" intf.intf_name meth.fun_name;
          fun_res = meth.fun_res;
          fun_params =
            ("this", In, Type_named "llinterface") :: meth.fun_params;
          fun_call = None } in
      Funct.ml_declaration oc prim)
    intf.intf_methods;
  fprintf oc "\n";
  (* Define the internal wrapper class (taking a Com.interface as argument) *)
  fprintf oc "class %s_internal (llintf : Com.interface) =\n" intfname;
  fprintf oc "  object\n";
  begin match intf.intf_super with
    None ->
      fprintf oc "    val cintf = llintf\n"
  | Some s ->
      fprintf oc "    inherit (%s_internal llintf)\n" (String.uncapitalize s.intf_name)
  end;
  List.iter
    (fun meth ->
      let methname = String.uncapitalize meth.fun_name in
      fprintf oc "    method %s = %s_%s cintf\n"
              methname intfname meth.fun_name)
    intf.intf_methods;
  fprintf oc "  end\n\n";
  (* Register the constructor for this class so that it can be called from C *)
  fprintf oc
    "let _ = Callback.register \"new %s.%s_internal\" (new %s_internal)\n\n"
    !module_name intf.intf_name intfname;
  (* Define the public wrapper class (taking a Com.iUnknown as argument) *)
  fprintf oc "class %s_class (intf : #Com.iUnknown) = \n" intfname;
  fprintf oc "  %s_internal (intf#queryInterface \"%s\")\n\n"
             intfname (String.escaped intf.intf_uid)

(* If heap allocation is needed, set up null allocation arena (= indefinite
   extent) *)

let output_arena before after =
  if !need_deallocation then begin
    fprintf before "  camlidl_arena * _arena = NULL;\n"
  end

(* Generate callback wrapper for calling an ML method from C *)

let emit_callback_wrapper oc intf meth =
  current_function := sprintf "%s::%s" intf.intf_name meth.fun_name;
  need_deallocation := false;
  let (ins, outs) = ml_view meth in
  (* Emit function header *)
  let fun_name =
    sprintf "camlidl_%s_%s_%s_callback"
            !module_name intf.intf_name meth.fun_name in
  fprintf oc "static %a(" out_c_decl (fun_name, meth.fun_res);
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
  iprintf pc "_vres = callbackN(Lookup(_varg[0], _vlabel), %d, _varg);\n"
             (num_ins + 1);
             (* FIXME: escaping exceptions *)
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
  output_arena oc pc;
  (* Return result if any *)
  if meth.fun_res <> Type_void then
    iprintf pc "return _res;\n";
  output_variable_declarations oc;
  end_diversion oc;
  fprintf oc "}\n\n"

(* Generate the vtable for an interface (for ml2c conversion) *)

let emit_vtable oc intf =
  let rec emit_vtbl intf =
    (* Temporary hack for IUnknown *)
    if intf.intf_name = "IUnknown" then begin
      fprintf oc "  (void *) camlidl_unknwn_IUnknown_QueryInterface_callback,\n";
      fprintf oc "  (void *) camlidl_unknwn_IUnknown_AddRef_callback,\n";
      fprintf oc "  (void *) camlidl_unknwn_IUnknown_Release_callback,\n";
    end else begin
      begin match intf.intf_super with
        None -> ()
      | Some s -> emit_vtbl s
      end;
      List.iter
        (fun m -> fprintf oc "  /* %s */ (void *) camlidl_%s_%s_%s_callback,\n"
                          m.fun_name !module_name intf.intf_name m.fun_name)
        intf.intf_methods
    end in
  fprintf oc "struct %sVtbl camlidl_%s_%s_vtbl = {\n"
             intf.intf_name !module_name intf.intf_name;
  emit_vtbl intf;
  fprintf oc "};\n\n"

(* Generate ml2c function for an interface *)

let emit_ml2c oc intf =
  fprintf oc "void camlidl_ml2c_%s_interface_%s(value vobj, struct camlidl_intf * intf, camlidl_arena * _arena)\n"
             !module_name intf.intf_name;
  fprintf oc "{\n";
  fprintf oc "  camlidl_make_interface(&camlidl_%s_%s_vtbl, vobj, intf, &IID_%s);\n"
             !module_name intf.intf_name intf.intf_name;
  fprintf oc "}\n\n"

(* Generate c2ml function for an interface *)

let emit_c2ml oc intf =
  fprintf oc "value camlidl_c2ml_%s_interface_%s(interface %s * intf)\n"
             !module_name intf.intf_name intf.intf_name;
  fprintf oc "{\n";
  fprintf oc "  static value * clos_new = NULL;\n";
  fprintf oc "  value vintf;\n";
  fprintf oc "  if (intf->lpVtbl == &camlidl_%s_%s_vtbl)\n"
             !module_name intf.intf_name;
  fprintf oc "    return ((struct camlidl_intf *) intf)->caml_object;\n";
  fprintf oc "  if (clos_new == NULL)\n";
  fprintf oc "    clos_new = caml_named_value(\"new %s.%s_internal\");\n"
             !module_name intf.intf_name;
  fprintf oc "  vintf = camlidl_pack_interface(intf);\n";
  fprintf oc "  return callback(*clos_new, vintf);\n";
  fprintf oc "}\n\n"

(* Forward declaration of the translation functions *)

let declare_transl oc intf =
  fprintf oc "interface %s * camlidl_ml2c_%s_interface_%s(value vobj, camlidl_arena * _arena);\n"
             intf.intf_name !module_name intf.intf_name;
  fprintf oc "value camlidl_c2ml_%s_interface_%s(interface %s * intf);\n"
             !module_name intf.intf_name intf.intf_name

(* Definition of the translation functions *)

let emit_transl oc intf =
  (* Temporary hack for IUnknown *)
  if intf.intf_name <> "IUnknown" then
    List.iter (emit_callback_wrapper oc intf) intf.intf_methods;
  emit_vtable oc intf;
  emit_ml2c oc intf;
  List.iter (Funct.emit_method_wrapper oc intf.intf_name) intf.intf_methods;
  emit_c2ml oc intf

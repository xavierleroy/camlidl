open Printf
open Utils
open Idltypes
open Cvttyp

(* Generate temporaries *)

let var_counter = ref 0
let temp_variables = ref([] : (string * idltype) list)

let new_var prefix =
  incr var_counter;
  prefix ^ string_of_int !var_counter
  
let new_c_variable ty =
  let name = new_var "_c" in
  temp_variables := (name, ty) :: !temp_variables;
  name

let new_ml_variable () =
  let name = new_var "_v" in
  temp_variables := (name, Type_named "value") :: !temp_variables;
  name

let output_variable_declarations oc =
  List.iter
    (fun name_ty -> iprintf oc "%a;\n" out_c_decl name_ty)
    (List.rev !temp_variables);
  temp_variables := [];
  var_counter := 0

(* Copy an array of values into the fields of a newly-allocated block *)

let copy_values_to_block oc src dst numvals =
  if numvals <= 4 then
    for i = 0 to numvals - 1 do
      iprintf oc "Field(%s, %d) = %s[%d];\n" dst i src i
    done
  else begin
    let idx = new_var "_c" in
    iprintf oc "{ mlsize_t %s;\n" idx;
    increase_indent();
    iprintf oc "for (%s = 0; %s < %d; %s++) Field(%s, %s) = %s[%s];\n"
               idx idx numvals idx dst idx src idx;
    decrease_indent();
    iprintf oc "}\n"
  end

(* Keep track of variables allocated with stat_alloc *)

let to_deallocate = ref ([] : string list)

let add_to_deallocate v =
  to_deallocate := v :: !to_deallocate

let output_deallocates oc =
  List.iter (fun v -> iprintf oc "stat_free(%s);\n" v) !to_deallocate;
  to_deallocate := []

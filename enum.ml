(* Handling of enums *)

open Printf
open Utils
open Variables
open Idltypes

(* Translate an ML datatype [v] to a C enum [c] *)

let enum_ml_to_c ml_to_c oc en v c =
  iprintf oc "%s = _transl_table_enum_%d[Int_val(%s)];\n" c en.en_stamp v

(* Translate a C enum [c] to an ML datatype [v] *)

let enum_c_to_ml c_to_ml oc en c v =
  if List.length en.en_consts <= 4 then begin
    iprintf oc "switch(%s) {\n" c;
    iter_index
      (fun i cst -> iprintf oc "case %s: %s = Val_int(%d); break;\n" cst v i)
      0 en.en_consts;
    iprintf oc "default: invalid_argument(\"%s: bad enum %s value\");\n"
               !current_function en.en_name;
    iprintf oc "}\n"
  end else begin
    iprintf oc "%s = camlidl_find_enum(%s, _transl_table_enum_%d, %d, \
                                       \"%s: bad enum %s value\");\n"
               v c en.en_stamp (List.length en.en_consts)
               !current_function en.en_name
  end

(* Translate an ML list [v] to a C enumset [c] *)

let enumset_ml_to_c ml_to_c oc en v c =
  if en.en_name = "" then
    error "[set] attribute does not apply to anonymous enum";
  iprintf oc "%s = convert_flag_list(%s, _transl_table_enum_%d);\n"
             c v en.en_stamp

(* Translate a C enumset [c] to an ML list [v] *)

let enumset_c_to_ml c_to_ml oc en c v =
  if en.en_name = "" then
    error "[set] attribute does not apply to anonymous enum";
  iprintf oc "%s = camlidl_alloc_flag_list(%s, _transl_table_enum_%d, %d);\n"
             v c en.en_stamp (List.length en.en_consts)

(* Handling of enums *)

open Printf
open Utils
open Variables
open Idltypes

(* Translate an ML datatype [v] to a C enum [c] *)

let enum_ml_to_c ml_to_c oc en v c =
  iprintf oc "%s = _enum_transl_tablee_%d[Int_val(%s)];\n" c en.en_stamp v

(* Translate a C enum [c] to an ML datatype [v] *)

let enum_c_to_ml c_to_ml oc en c v =
  if List.length en.en_consts <= 4 then begin
    iprintf oc "  switch(%s) {\n" c;
    map_index
      (fun i cst -> iprintf oc "case %s: %s = Val_int(%d); break;\n" cst v i)
      0 en.en_consts;
    iprintf oc "}\n"
  end else begin
    let i = new_c_variable (Type_int Int) in
    iprintf oc "for (%s = 0; _enum_transl_table_%d[%s] != %s; %s++)\n"
               i en.en_stamp i c i;
    iprintf oc "  if (%s >= %d) invalid_argument(\"%s: bad enum %s value\");\n"
               i (List.length en.en_consts) !current_function en.en_name;
    iprintf oc "%s = Val_int(%s);\n" v i
  end

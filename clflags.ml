(* Command-line flags *)

let search_path = ref [Filename.current_dir_name] (* -I *)
let include_header = ref true   (* -no-include *)
let gen_header = ref false      (* -make-header *)
let prepro_defines = ref ["CAMLIDL"] (* -D *)
let use_cpp = ref true (* -cpp / -nocpp *)
let preprocessor = ref Config.cpp (* -prepro *)


(* Command-line flags *)

let search_path = ref [Filename.current_dir_name] (* -I *)
let include_header = ref true   (* -header / -noheader *)
let prepro_defines = ref ["CAMLIDL"] (* -D *)
let use_cpp = ref true (* -cpp / -nocpp *)
let preprocessor = ref Config.cpp (* -prepro *)


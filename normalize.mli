(* Normalization of IDL types after parsing *)

val process_file: string -> File.idl_file * File.idl_file * File.idl_file
  (* First result: normalized interface
     Second result: list of all imports
     Third result: list of all type definitions, including
       anonymous structs, enums, unions. *)

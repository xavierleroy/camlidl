(* Normalization of IDL types after parsing *)

val process_file: string -> File.components * File.components
  (* First result: normalized components of file
     Second result: normalized components for all imports. *)

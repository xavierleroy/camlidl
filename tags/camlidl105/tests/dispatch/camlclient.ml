(* A simple client in Caml *)

open Printf

let testcomp clsid =
  print_string "Call Com.create_instance to create component and get interface IX"; print_newline();
  begin try
    let ix = Com.create_instance clsid Component.iid_iX in
    let obj = Component.use_iX ix in
    print_string "Calling Fx..."; print_newline();
    obj#fx;
    print_string "Calling FxStringIn(\"foo bar\")..."; print_newline();
    obj#fxStringIn("foo bar");
    print_string "Calling FxStringOut..."; print_newline();
    let res = obj#fxStringOut in
    print_string "Result is: "; print_string res; print_newline();
    print_string "Calling FxFakeError..."; print_newline();
    begin try
      obj#fxFakeError;
      print_string "FxFakeError returned normally"; print_newline()
    with Com.Error(_, who, what) ->
      print_string "Exception Com.Error("; print_string who;
      print_string ", "; print_string what; print_string ")"; print_newline()
    end
  with Com.Error(_, src, msg) ->
    printf "COM error (%s): %s\n" src msg; flush stdout
  end

let clsid_component1 = Com.clsid "0C092C2C-882C-11CF-A6BB-0080C7B2D682"
let clsid_component2 = Com.clsid "6a3d0750-dad9-11d2-8e2c-0060974fbf19"

let _ =
  Com.initialize();
  testcomp clsid_component1;
  testcomp clsid_component2;
  Com.uninitialize()

(* A simple client in Caml *)

open Printf

let testcomp clsid =
  print_string "Call Com.create_instance to create component and get interface IX"; print_newline();
  begin try
    let ix = Com.create_instance clsid Component.iid_iX in
    print_string "Calling Fx..."; print_newline();
    (Component.use_iX ix)#fx;
    begin try
      print_string "Ask for interface IY"; print_newline();
      let iy = Com.query_interface ix Component.iid_iY in
      print_string "Got it, calling Fy(5)..."; print_newline();
      (Component.use_iY iy)#fy 5
    with Com.Error(_, src, msg) ->
      printf "COM error (%s): %s\n" src msg; flush stdout
    end;
    begin try
      print_string "Ask for interface IZ"; print_newline();
      let iz = Com.query_interface ix Component.iid_iZ in
      print_string "Got it, calling Fz(12)..."; print_newline();
      let res = (Component.use_iZ iz)#fz 12 in
      printf "Return value is %d\n" res; flush stdout
    with Com.Error(_, src, msg) ->
      printf "COM error (%s): %s\n" src msg; flush stdout
    end
  with Com.Error(_, src, msg) ->
    printf "COM error (%s): %s\n" src msg; flush stdout
  end

let clsid_component1 = Com.clsid "0c092c21-882c-11cf-a6bb-0080c7b2d682"
let clsid_component2 = Com.clsid "aab56090-c721-11d2-8e2b-0060974fbf19"

let _ =
  Com.initialize();
  testcomp clsid_component1;
  testcomp clsid_component2;
  Com.uninitialize()

(* Test the Com component *)

let test_ix c =
  print_string "Testing IX interface..."; print_newline();
  print_string "Calling f 10"; print_newline();
  c#f 10

let test_iy c =
  print_string "Testing IY interface..."; print_newline();
  print_string "Calling g 2"; print_newline();
  let n = c#g 2 in
  print_string "Result is "; print_int n; print_newline()

class my_ix =
  object
    method queryInterface (x : string) = (failwith "my_ix : queryInterface" : Com.interface)
    method addRef = (failwith "my_ix : addRef" : int)
    method release = (failwith "my_ix : release" : int)
    method f x =
      print_string "my_ix: f "; print_int x; print_newline()
  end

class my_iy =
  object
    method queryInterface (x : string) = (failwith "my_iy : queryInterface" : Com.interface)
    method addRef = (failwith "my_iy : addRef" : int)
    method release = (failwith "my_iy : release" : int)
    method g x =
      print_string "my_iy: g "; print_int x; print_newline();
      let res = x / 2 in
      print_string "my_iy: g returns "; print_int res; print_newline();
      res
  end

let _ =
  let c = Component.create_instance() in
  begin try
    test_ix (new Component.iX_class c)
  with Failure s ->
    print_string "Lookup of IX interface failed: ";
    print_string s; print_newline()
  end;
  begin try
    test_iy (new Component.iY_class c)
  with Failure s ->
    print_string "Lookup of IY interface failed: ";
    print_string s; print_newline()
  end;
  Component.test_ix (new my_ix);
  Component.test_iy (new my_iy);
  Gc.full_major()



  

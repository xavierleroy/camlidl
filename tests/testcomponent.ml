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
    method f x =
      print_string "my_ix: f "; print_int x; print_newline()
  end

class my_iy =
  object
    method g x =
      print_string "my_iy: g "; print_int x; print_newline();
      let res = x / 2 in
      print_string "my_iy: g returns "; print_int res; print_newline();
      res
  end

let make_test() =
  let c = Component.create_instance() in
  begin try
    test_ix (Component.use_iX (Com.queryInterface c Component.iid_iX))
  with Com.Error s ->
    print_string "Lookup of IX interface failed: ";
    print_string s; print_newline()
  end;
  begin try
    test_iy (Component.use_iY (Com.queryInterface c Component.iid_iY))
  with Com.Error s ->
    print_string "Lookup of IY interface failed: ";
    print_string s; print_newline()
  end;
  let cx = Component.make_iX (new my_ix)
  and cy = Component.make_iY (new my_iy) in
  Component.test_ix cx;
  Component.test_iy cy;
  Component.test_component(Com.iUnknown_of(Com.aggregate cx cy))

let _ =
  make_test();
  Gc.full_major()

(* A simple component in Caml *)

class mycomponent =
  object
    method fx =
      print_string "Camlcomp: fx"; print_newline()
    method fy n =
      print_string "Camlcomp: fy "; print_int n; print_newline()
    method fz n =
      print_string "Camlcomp: fz "; print_int n; print_newline();
      n / 2
  end

let factory () =
  print_string "Camlcomp: factory is called"; print_newline();
  let obj = new mycomponent in
  let ix = Component.make_iX obj
  and iy = Component.make_iY obj
  and iz = Component.make_iZ obj in
  Com.combine (Com.combine ix iy) iz

let _ =
  Com.register_factory
    { Com.create = factory;
      Com.clsid = Com.clsid "aab56090-c721-11d2-8e2b-0060974fbf19";
      Com.friendly_name = "CAMLIDL, test component 1";
      Com.ver_ind_prog_id = "CAMLIDL.Testcomp1";
      Com.prog_id = "CAMLIDL.Testcomp1.1" }


(* A simple component in Caml *)

class mycomponent =
  object
    method fx =
      (*print_string "Camlcomp: fx"; print_newline(); *) ()
    method fxStringIn str =
      print_string "Camlcomp: fxStringIn "; print_string str; print_newline()
    method fxStringOut =
      (*print_string "Camlcomp: fxStringOut "; print_newline();*)
      "This string comes from Caml"
    method fxFakeError =
      (failwith "FxFakeError" : unit)
  end

let factory () =
  (*print_string "Camlcomp: factory is called"; print_newline();*)
  Component.make_iX(new mycomponent)

let _ =
  Com.register_factory
    { Com.create = factory;
      Com.clsid = Com.clsid "6a3d0750-dad9-11d2-8e2c-0060974fbf19";
      Com.friendly_name = "CAMLIDL, test component 2";
      Com.ver_ind_prog_id = "CAMLIDL.Testcomp2";
      Com.prog_id = "CAMLIDL.Testcomp2.1" }


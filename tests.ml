let f1 = { fun_name = "f1"; fun_res = Type_int Int;
           fun_params = ["x", In, Type_int Int] };;
let f2 = { fun_name = "f2"; fun_res = Type_int Int;
           fun_params = ["x", In, Type_int ULong] };;
let f3 = { fun_name = "f3"; fun_res = Type_void;
           fun_params = ["p", Out,
                         Type_pointer({ptrkind=Ref;ignore=false}, Type_int Int)]}
let f4 = { fun_name = "f4"; fun_res = Type_void;
           fun_params = ["p", InOut,
                         Type_pointer({ptrkind=Ref;ignore=false}, Type_int Int)]}
let f5 = { fun_name = "f4"; fun_res = Type_int Int;
           fun_params = ["x", In, Type_int Int;
                         "p", InOut,
                         Type_pointer({ptrkind=Ref;ignore=false}, Type_int Int)]}

let f6 =
  { fun_name = "f6";
    fun_res = Type_pointer({ptrkind=Unique;ignore=false}, Type_int Int);
    fun_params = ["x", In,
                  Type_pointer({ptrkind=Unique;ignore=false}, Type_int Int)] }

let f7 =
  { fun_name = "f7";
    fun_res = Type_void;
    fun_params = ["t", InOut,
                  Type_array({bound=Some 10; size=None; length=None; is_string=false}, Type_int Int)] }

let f8 =
  { fun_name = "f8";
    fun_res = Type_void;
    fun_params = ["n", In, Type_int Int;
                  "t", InOut,
                  Type_array({bound=None; size=Some(Var "n"); length=None; is_string=false}, Type_int Int)] }

let f9 =
  { fun_name = "f9";
    fun_res = Type_void;
    fun_params = ["n", In, Type_int Int;
                  "t", InOut,
                  Type_array({bound=Some 10; size=Some(Var "n"); length=None; is_string=false}, Type_int Int)] }

let f10 =
  { fun_name = "f10";
    fun_res = Type_void;
    fun_params = ["n", In, Type_int Int;
                  "m", Out, Type_pointer({ptrkind=Ref;ignore=false}, Type_int Int);
                  "t", InOut,
                  Type_array({bound=Some 10; size=Some(Var "n"); length=Some(Deref "m"); is_string=false}, Type_int Int)] }

let f11 = { fun_name = "f11"; fun_res = Type_void;
           fun_params = ["p", InOut,
                         Type_pointer({ptrkind=Ref;ignore=false}, Type_int Int);
                        "q", InOut,
                         Type_pointer({ptrkind=Ref;ignore=false}, Type_int Int)]}

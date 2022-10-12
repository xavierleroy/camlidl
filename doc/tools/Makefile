all: transf texquote2

%: %.ml
	ocamlopt -o $* $*.ml

transf.ml: transf.mll
	ocamllex transf.mll

clean:
	rm -f transf texquote2 *.cm? *.o transf.ml


OCAMLC=ocamlc -g
OCAMLDEP=ocamldep
OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex

OBJS=utils.cmo cvttyp.cmo variables.cmo \
  struct.cmo enum.cmo union.cmo cvtval.cmo \
  funct.cmo structdecl.cmo enumdecl.cmo uniondecl.cmo typedef.cmo \
  intfgen.cmo stubgen.cmo \
  parser_simple.cmo lexer_simple.cmo normalize.cmo \
  main.cmo

camlidl: $(OBJS)
	$(OCAMLC) -o camlidl $(OBJS)

parser_simple.ml parser_simple.mli: parser_simple.mly
	$(OCAMLYACC) parser_simple.mly

clean::
	rm -f parser_simple.ml parser_simple.mli

beforedepend:: parser_simple.ml parser_simple.mli

lexer_simple.ml: lexer_simple.mll
	$(OCAMLLEX) lexer_simple.mll

clean::
	rm -f lexer_simple.ml

beforedepend:: lexer_simple.ml

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) -c $<

.mli.cmi:
	$(OCAMLC) -c $<

.ml.cmx:
	$(OCAMLOPT) -c $<

# Clean up
clean::
	rm -f *.cm[iox]

# Dependencies
depend: beforedepend
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend

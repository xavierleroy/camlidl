include ../config/Makefile

OBJS=config.cmo utils.cmo ebuff.cmo clflags.cmo \
  lexpr.cmo cvttyp.cmo variables.cmo \
  array.cmo struct.cmo enum.cmo union.cmo cvtval.cmo \
  structdecl.cmo enumdecl.cmo uniondecl.cmo \
  typedef.cmo funct.cmo constdecl.cmo intf.cmo \
  file.cmo predef.cmo \
  parse_aux.cmo parser_midl.cmo lexer_midl.cmo linenum.cmo parse.cmo \
  normalize.cmo \
  main.cmo

PROG=camlidl$(EXE)

all: $(PROG)

$(PROG): $(OBJS)
	$(OCAMLC) -o $(PROG) $(OBJS)

clean::
	rm -f $(PROG)

parser_midl.ml parser_midl.mli: parser_midl.mly
	$(OCAMLYACC) parser_midl.mly

clean::
	rm -f parser_midl.ml parser_midl.mli parser_midl.output

beforedepend:: parser_midl.ml parser_midl.mli

lexer_midl.ml: lexer_midl.mll
	$(OCAMLLEX) lexer_midl.mll

clean::
	rm -f lexer_midl.ml

beforedepend:: lexer_midl.ml

config.ml: config.mlp ../config/Makefile
	-rm -f config.ml
	sed -e 's|%%CPP%%|$(CPP)|' \
          config.mlp > config.ml
	-chmod -w config.ml

clean::
	rm -f config.ml

beforedepend:: config.ml

linenum.ml: linenum.mll
	$(OCAMLLEX) linenum.mll

clean::
	rm -f linenum.ml

beforedepend:: linenum.ml

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) -c $<

.mli.cmi:
	$(OCAMLC) -c $<

.ml.cmx:
	$(OCAMLOPT) -c $<

# Install
install:
	cp $(PROG) $(BINDIR)

# Clean up
clean::
	rm -f *.cm[iox] *~

# Dependencies
depend: beforedepend
	$(OCAMLDEP) *.mli *.ml > .depend

include .depend

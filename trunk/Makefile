OCAMLC=ocamlc -g
OCAMLDEP=ocamldep
OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex
CC=gcc
CFLAGS=-O -Wall -I/usr/local/lib/ocaml

OBJS=utils.cmo cvttyp.cmo variables.cmo \
  array.cmo struct.cmo enum.cmo union.cmo cvtval.cmo \
  funct.cmo structdecl.cmo enumdecl.cmo uniondecl.cmo typedef.cmo \
  intfgen.cmo stubgen.cmo \
  parser_simple.cmo lexer_simple.cmo normalize.cmo \
  main.cmo

COBJS=camlidlruntime.o

all: camlidl libcamlidl.a

camlidl: $(OBJS)
	$(OCAMLC) -o camlidl $(OBJS)

clean::
	rm -f camlidl

libcamlidl.a: $(COBJS)
	rm -f libcamlidl.a
	ar rc libcamlidl.a $(COBJS)

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
	rm -f *.cm[iox] *.[oa]

# Dependencies
depend: beforedepend
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend
	$(CC) $(CFLAGS) -MM *c >> .depend

include .depend

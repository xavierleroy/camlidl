OCAMLC=ocamlc -g
OCAMLDEP=ocamldep
OCAMLYACC=ocamlyacc -v
OCAMLLEX=ocamllex
CC=gcc
CFLAGS=-g -O -Wall -I/usr/local/lib/ocaml

OBJS=utils.cmo ebuff.cmo lexpr.cmo cvttyp.cmo variables.cmo \
  array.cmo struct.cmo enum.cmo union.cmo cvtval.cmo \
  structdecl.cmo enumdecl.cmo uniondecl.cmo \
  typedef.cmo funct.cmo constdecl.cmo intf.cmo \
  file.cmo \
  parser_midl.cmo lexer_midl.cmo normalize.cmo \
  main.cmo

COBJS=camlidlruntime.o

all: camlidl libcamlidl.a com.cmo

camlidl: $(OBJS)
	$(OCAMLC) -o camlidl $(OBJS)

clean::
	rm -f camlidl

libcamlidl.a: $(COBJS)
	rm -f libcamlidl.a
	ar rc libcamlidl.a $(COBJS)

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
	$(CC) $(CFLAGS) -MM *.c >> .depend

include .depend

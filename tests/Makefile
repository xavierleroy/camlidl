include ../config/Makefile

CAMLIDL=../compiler/camlidl
CAMLC=ocamlc -I ../lib
CC=gcc
CFLAGS=-Wall -I.. -I$(OCAMLLIB)
CCPP=egcs
CPPFLAGS=-Wall -I.. -I$(OCAMLLIB)

TESTS=basics.idl arrays.idl structs.idl unions.idl unix.idl

all: ../caml/camlidlruntime.h $(TESTS:.idl=.o) testcomp

$(TESTS:.idl=.o) component.o: $(CAMLIDL) ../runtime/camlidlruntime.h

unix.o: unix.idl
	$(CAMLIDL) -no-include unix.idl
	$(CAMLC) -c unix.mli
	$(CAMLC) -c unix.ml
	$(CC) $(CFLAGS) -c unix.c

testcomp: comcomp.o component.o component.cmo testcomponent.cmo \
          ../runtime/libcamlidl.a
	$(CAMLC) -cc $(CCPP) -custom -o testcomp \
                com.cmo comcomp.o component.o component.cmo testcomponent.cmo \
                ../runtime/libcamlidl.a

component.cmo testcomponent.cmo: component.cmi
component.cmo component.cmi: component.o

../caml/camlidlruntime.h:
	mkdir ../caml
	ln -s ../runtime/camlidlruntime.h ../caml

.SUFFIXES:
.SUFFIXES: .idl .o .cpp .ml .mli .cmo .cmi

.idl.o:
	$(CAMLIDL) -header $*.idl
	$(CAMLC) -c $*.mli
	$(CAMLC) -c $*.ml
	$(CC) $(CFLAGS) -c $*.c

.cpp.o:
	$(CCPP) $(CPPFLAGS) -c $*.cpp

.ml.cmo:
	$(CAMLC) -c $*.ml

.mli.cmi:
	$(CAMLC) -c $*.mli

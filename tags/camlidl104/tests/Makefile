include ../config/Makefile

CAMLIDL=../compiler/camlidl
CAMLC=ocamlc -I ../lib
INCLUDES=-I.. -I$(OCAMLLIB)
CCPP=$(CC)
CPPFLAGS=$(CFLAGS)

TESTS=basics.idl arrays.idl structs.idl unions.idl typedefs.idl $(TESTS_$(OSTYPE))
TESTS_unix=unix.idl
COMLIBS=$(COMLIBS_$(OSTYPE))
COMLIBS_win32=advapi32.lib ole32.lib oleaut32.lib

all: ../caml/camlidlruntime.h $(TESTS:.idl=.$(OBJEXT)) testcomp

$(TESTS:.idl=.$(OBJEXT)) component.$(OBJEXT): $(CAMLIDL) ../runtime/camlidlruntime.h

unix.$(OBJEXT): unix.idl
	$(CAMLIDL) -no-include unix.idl
	$(CAMLC) -c unix.mli
	$(CAMLC) -c unix.ml
	$(CC) $(CFLAGS) $(INCLUDES) -c unix_stubs.c
	mv unix_stubs.$(OBJEXT) unix.$(OBJEXT)

testcomp: comcomp.$(OBJEXT) component.$(OBJEXT) component.cmo testcomponent.cmo \
          ../runtime/libcamlidl.$(LIBEXT)
	$(CAMLC) -cc $(CCPP) -custom -o testcomp \
                com.cmo comcomp.$(OBJEXT) component.$(OBJEXT) \
		component.cmo testcomponent.cmo \
                ../runtime/libcamlidl.$(LIBEXT) $(COMLIBS)

clean::
	rm -f testcomp

component.cmo testcomponent.cmo: component.cmi
component.cmo component.cmi: component.$(OBJEXT)

../caml/camlidlruntime.h:
	mkdir ../caml
	ln -s ../runtime/camlidlruntime.h ../caml

.SUFFIXES:
.SUFFIXES: .idl .$(OBJEXT) .cpp .ml .mli .cmo .cmi

.idl.$(OBJEXT):
	$(CAMLIDL) -header $*.idl
	$(CAMLC) -c $*.mli
	$(CAMLC) -c $*.ml
	$(CC) $(CFLAGS) $(INCLUDES) -c $*_stubs.c
	mv $*_stubs.$(OBJEXT) $*.$(OBJEXT)

.cpp.$(OBJEXT):
	$(CCPP) $(CPPFLAGS) $(INCLUDES) -c $*.cpp

.ml.cmo:
	$(CAMLC) -c $*.ml

.mli.cmi:
	$(CAMLC) -c $*.mli

clean::
	rm -f $(TESTS:.idl=.mli) $(TESTS:.idl=.ml) $(TESTS:.idl=.h) $(TESTS:.idl=_stubs.c)

clean::
	rm -f *.cm[io] *.$(OBJEXT)

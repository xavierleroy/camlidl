include ../config/Makefile

BYTEOBJS=com.cmo
BYTELIB=com.cma
NATIVEOBJS=$(BYTEOBJS:.cmo=.cmx)
NATIVELIB=$(BYTELIB:.cma=.cmxa)
INTERFACES=$(BYTEOBJS:.cmo=.cmi)

all: $(BYTELIB) $(NATIVELIB)

$(BYTELIB): $(BYTEOBJS)
	$(OCAMLC) -a -o $(BYTELIB) $(BYTEOBJS)

$(NATIVELIB): $(NATIVEOBJS)
	$(OCAMLOPT) -a -o $(NATIVELIB) $(NATIVEOBJS)

install:
	cp $(INTERFACES) $(BYTELIB) $(NATIVELIB) $(NATIVELIB:.cmxa=.a) $(OCAMLLIB)
	cd $(OCAMLLIB); $(RANLIB) $(NATIVELIB:.cmxa=.a)

.SUFFIXES: .mli .ml .cmi .cmo .cmx

.mli.cmi:
	$(OCAMLC) -c $<
.ml.cmo:
	$(OCAMLC) -c $<
.ml.cmx:
	$(OCAMLOPT) -c $<

# Clean up
clean::
	rm -f *.cm[ioax] $.cmxa *~

# Dependencies
depend:
	$(OCAMLDEP) *.mli *.ml > .depend

include .depend

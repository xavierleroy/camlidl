#***********************************************************************
#*                                                                     *
#*                              CamlIDL                                *
#*                                                                     *
#*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *
#*                                                                     *
#*  Copyright 1999 Institut National de Recherche en Informatique et   *
#*  en Automatique.  All rights reserved.  This file is distributed    *
#*  under the terms of the GNU Library General Public License.         *
#*                                                                     *
#***********************************************************************

#* $Id: Makefile,v 1.3 2000-08-19 11:04:59 xleroy Exp $

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
	cp $(INTERFACES) $(BYTELIB) $(NATIVELIB) $(NATIVELIB:.cmxa=$(LIB)) $(OCAMLLIB)
	cd $(OCAMLLIB); $(RANLIB) $(NATIVELIB:.cmxa=$(LIB))

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

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

#* $Id: Makefile,v 1.4 2002-04-22 11:50:47 xleroy Exp $

include ../config/Makefile

BYTEOBJS=com.cmo
BYTELIB=com.cma
NATIVEOBJS=$(BYTEOBJS:.cmo=.cmx)
NATIVELIB=$(BYTELIB:.cma=.cmxa)
INTERFACES=$(BYTEOBJS:.cmo=.cmi)

all: $(BYTELIB) $(NATIVELIB)

$(BYTELIB): $(BYTEOBJS)
	$(OCAMLC) -a -o $(BYTELIB) -dllib -lcamlidl -cclib -lcamlidl $(BYTEOBJS)

$(NATIVELIB): $(NATIVEOBJS)
	$(OCAMLOPT) -a -o $(NATIVELIB) -cclib -lcamlidl $(NATIVEOBJS)

install:
	cp $(INTERFACES) $(BYTELIB) $(NATIVELIB) $(NATIVELIB:.cmxa=.$(LIBEXT)) $(OCAMLLIB)
	cd $(OCAMLLIB); $(RANLIB) $(NATIVELIB:.cmxa=.$(LIBEXT))

.SUFFIXES: .mli .ml .cmi .cmo .cmx

.mli.cmi:
	$(OCAMLC) -c $<
.ml.cmo:
	$(OCAMLC) -c $<
.ml.cmx:
	$(OCAMLOPT) -c $<

# Clean up
clean::
	rm -f *.cm[ioax] *.cmxa *.[ao] *.lib *~

# Dependencies
depend:
	$(OCAMLDEP) *.mli *.ml > .depend

include .depend

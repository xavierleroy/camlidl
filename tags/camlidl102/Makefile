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

#* $Id: Makefile,v 1.18 2000-08-19 11:04:55 xleroy Exp $

include config/Makefile

all:
	cd compiler; $(MAKE) all
	cd runtime; $(MAKE) all
	cd lib; $(MAKE) all
	cd tools; $(MAKE) all

install:
	cd compiler; $(MAKE) install
	cd runtime; $(MAKE) install
	cd lib; $(MAKE) install
	cd tools; $(MAKE) install

clean:
	cd compiler; $(MAKE) clean
	cd runtime; $(MAKE) clean
	cd lib; $(MAKE) clean
	cd tools; $(MAKE) clean

depend:
	cd compiler; $(MAKE) depend
	cd runtime; $(MAKE) depend
	cd lib; $(MAKE) depend
	cd tools; $(MAKE) depend


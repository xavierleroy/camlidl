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


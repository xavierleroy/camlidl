ALLTESTS=\
  test001.idl test002.idl test003.idl test004.idl \
  test007.idl test008.idl test009.idl test010.idl \
  test011.idl test013.idl test014.idl test015.idl \
  test016.idl test017.idl test018.idl test019.idl test020.idl \
  test021.idl test022.idl test023.idl test024.idl test025.idl \
  test026.idl test027.idl test028.idl test029.idl test030.idl \
  test031.idl test032.idl test033.idl test034.idl test035.idl

CAMLIDL=../../camlidl
CAMLC=ocamlc -I ../..
CC=gcc
CFLAGS=-I../.. -I/usr/local/lib/ocaml -Wall

all: $(ALLTESTS:.idl=.o)

$(ALLTESTS:.idl=.o): $(CAMLIDL)

.SUFFIXES:
.SUFFIXES: .idl .o

.idl.o:
	$(CAMLIDL) -header $*.idl
	$(CAMLC) -c $*.mli
	$(CAMLC) -c $*.ml
	$(CC) $(CFLAGS) -c $*.c

clean:
	rm -f *.ml *.mli *.cm[io] *.[cho] *~

CAMLDOC=$${HOME}/csldoc
TEXINPUTS=.:$(CAMLDOC)/styles:$(CAMLDOC)/manual:
TRANSF=$(CAMLDOC)/tools/transf
TEXQUOTE=texquote3
HTMLGEN=$(CAMLDOC)/tools/htmlgen
HTMLCUT=$(CAMLDOC)/tools/htmlcut
HTMLTHREAD=$(CAMLDOC)/tools/htmlthread
TEXEXPAND=$(CAMLDOC)/tools/texexpand
FORMATINTF=$(CAMLDOC)/tools/format-intf

all: main.dvi htmlman/index.html

main.dvi: main.tex manual.tex com.tex
	TEXINPUTS=$(TEXINPUTS) latex main.tex

htmlman/index.html: main.htex manual.htex com.tex
	$(TEXEXPAND) main.htex | $(HTMLGEN) > manual.html
	cd htmlman; $(HTMLCUT) -article ../manual.html > index.html
	cd htmlman; $(HTMLTHREAD) node*.html

manual.tex: manual.etex
	$(TRANSF) < manual.etex | $(TEXQUOTE) > manual.tex

manual.htex: manual.etex
	$(TRANSF) -html < manual.etex > manual.htex

com.tex: ../lib/com.mli
	$(FORMATINTF) ../lib/com.mli > com.tex

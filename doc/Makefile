CAMLDOC=$${HOME}/csldoc
TEXINPUTS=.:$(CAMLDOC)/styles:$(CAMLDOC)/manual:
TRANSF=$(CAMLDOC)/tools/transf
TEXQUOTE=texquote3
HTMLGEN=$(CAMLDOC)/tools/htmlgen
HTMLCUT=$(CAMLDOC)/tools/htmlcut
HTMLTHREAD=$(CAMLDOC)/tools/htmlthread

all: main.dvi htmlman/index.html

main.dvi: main.tex manual.tex
	TEXINPUTS=$(TEXINPUTS) latex main.tex

htmlman/index.html: main.htex manual.htex
	cat main.htex manual.htex | $(HTMLGEN) > manual.html
	cd htmlman; $(HTMLCUT) ../manual.html > index.html
	cd htmlman; $(HTMLTHREAD) node*.html

manual.tex: manual.etex
	$(TRANSF) < manual.etex | $(TEXQUOTE) > manual.tex

manual.htex: manual.etex
	$(TRANSF) -html < manual.etex | sed -e 's/\\section/\\chapter/' > manual.htex


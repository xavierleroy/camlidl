CAMLDOC=$${HOME}/csldoc
TEXINPUTS=.:$(CAMLDOC)/styles:$(CAMLDOC)/manual:/usr/local/lib/hevea:
HEVEAINPUTS=-I $(CAMLDOC)/manual -I ..
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

htmlman/index.html: main.tex manual.tex com.tex
	cd htmlman; \
        hevea $(HEVEAINPUTS) -fix ../main.tex && \
	hacha main.html

manual.tex: manual.etex
	$(TRANSF) < manual.etex | $(TEXQUOTE) > manual.tex

com.tex: ../lib/com.mli
	$(FORMATINTF) ../lib/com.mli > com.tex

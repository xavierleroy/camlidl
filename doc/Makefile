TEXINPUTS=./styles:
TRANSF=tools/transf
TEXQUOTE=tools/texquote2

all: $(TRANSF) $(TEXQUOTE) main.pdf htmlman/index.html

main.pdf: main.tex manual.tex
	TEXINPUTS=$(TEXINPUTS) latexmk -pdf main.tex

htmlman/index.html: main.tex manual.tex
	cd htmlman; \
        hevea -I ../styles -I .. -fix ../main.tex && \
	hacha main.html

manual.tex: manual.etex
	$(TRANSF) < manual.etex | $(TEXQUOTE) > manual.tex

$(TRANSF) $(TEXQUOTE):
	make -C tools

TEXINPUTS=./styles:
TRANSF=tools/transf
TEXQUOTE=tools/texquote2

all: $(TRANSF) $(TEXQUOTE) main.pdf ../docs/index.html

main.pdf: main.tex manual.tex
	TEXINPUTS=$(TEXINPUTS) latexmk -pdf main.tex

../docs/index.html: main.tex manual.tex
	cd ../docs && \
        hevea -I ../doc/styles -I ../doc -fix ../doc/main.tex && \
	hacha main.html

manual.tex: manual.etex
	$(TRANSF) < manual.etex | $(TEXQUOTE) > manual.tex

$(TRANSF) $(TEXQUOTE):
	make -C tools

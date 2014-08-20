HEADER =
OBJECT = search-zh-cn
SOURCE = $(OBJECT).tex
TEX = xelatex

.PHONY: all

all: pdf

image:
	make -C img

pdf: image
	$(TEX) $(OBJECT)
	$(TEX) $(OBJECT)

tex: pdf

html: image
	htlatex.bat $(OBJECT) "html, imgdir:img/"
	mv *.png img/

clean:
	rm -f *.aux *.dvi *.log *.out *.pdf *~

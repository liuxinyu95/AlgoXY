HEADER =
SOURCE = haskell-en.tex
OBJECT = haskell-en
TEX = xelatex

.PHONY: all

all: pdf

cpp:
	$(MAKE) cpp -C src

c:
	$(MAKE) c -C src

haskell:
	$(MAKE) haskell -C src

tool:
	$(MAKE) tool -C src

image:
	$(MAKE) -C img

#pdf: image
pdf:
	$(TEX) $(OBJECT)
	$(TEX) $(OBJECT)

tex: pdf

html: image
	htlatex.bat $(OBJECT) "html, imgdir:img/"
	mv *.png img/

clean:
	rm -f *.aux *.dvi *.log *.out *.pdf *~

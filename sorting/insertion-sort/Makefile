HEADER = 
SOURCE = isort-en.tex
OBJECT = isort-en
TEX = latex

.PHONY: all

all: pdf

cpp:
	make cpp -C src

c:
	make c -C src

haskell:
	make haskell -C src

image:
	make -C img

pdf: $(OBJECT).dvi
	dvipdfmx $(OBJECT)

tex: image
	latex $(OBJECT)
	latex $(OBJECT)

html: image
	htlatex.bat $(OBJECT) "html, imgdir:img/"
	mv *.png img/

clean:
	rm *.aux *.dvi *.log *.out *.pdf *~
HEADER = 
SOURCE = list-en.tex
OBJECT = list-en
OBJECT2 = list-cpptp-en
TEX = latex

.PHONY: all

all: pdf

cpp:
	make cpp -C src

c:
	make c -C src

haskell:
	make haskell -C src

tool:
	make tool -C src

image:
	make -C img

pdf: $(OBJECT).dvi
	dvipdfmx $(OBJECT)

tex: image
	latex $(OBJECT)
	latex $(OBJECT)

cpptp:
	latex $(OBJECT2)
	latex $(OBJECT2)

html: image
	htlatex.bat $(OBJECT) "html, imgdir:img/"
	mv *.png img/

clean:
	rm -f *.aux *.dvi *.log *.out *.pdf *~

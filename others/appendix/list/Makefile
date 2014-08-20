HEADER =
OBJECT = list-zh-cn
OBJECT2 = list-cpptp-en
SOURCE = $(OBJECT).tex
TEX = xelatex

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

pdf: image
	$(TEX) $(OBJECT)
	$(TEX) $(OBJECT)

tex: pdf

cpptp:
	$(TEX) $(OBJECT2)
	$(TEX) $(OBJECT2)

html: image
	htlatex.bat $(OBJECT) "html, imgdir:img/"
	mv *.png img/

clean:
	rm -f *.aux *.dvi *.log *.out *.pdf *~

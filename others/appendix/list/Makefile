HEADER =
OBJECT = list-zh-cn
OBJECT2 = list-cpptp-en
SOURCE = $(OBJECT).tex
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

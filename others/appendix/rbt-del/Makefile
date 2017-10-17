HEADER =
OBJECT = rbt-del-en
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

clean:
	rm -f *.aux *.dvi *.log *.out *.pdf *~

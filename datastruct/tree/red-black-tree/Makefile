HEADER =
SOURCE = rbtree-zh-cn.tex
OBJECT = rbtree-zh-cn
TEX = xelatex

.PHONY: all

all: pdf

cpp:
	$(MAKE) cpp -C src

haskell:
	$(MAKE) haskell -C src

image:
	$(MAKE) -C img

pdf: image
	$(TEX) $(OBJECT)
	$(TEX) $(OBJECT)

tex: pdf

html:
	htlatex $(OBJECT) "html, imgdir:img/"
	mv *.png img/

clean:
	rm -f *.4ct *.4tc *.aux *.css *.dvi *.html
	rm -f *.idv *.lg *.log *.out *.pdf *.tmp *.xref *.cache *.zip *~

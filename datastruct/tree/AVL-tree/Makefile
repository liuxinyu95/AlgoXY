HEADER =
SOURCE = avltree-zh-cn.tex
OBJECT = avltree-zh-cn
TEX = xelatex

.PHONY: all

all: pdf

cpp:
	make cpp -C src

haskell:
	make haskell -C src

image:
	make -C img

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

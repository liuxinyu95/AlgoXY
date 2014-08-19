HEADER =
SOURCE = bstree-zh-cn.tex
OBJECT = bstree-zh-cn
TEX = xelatex

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

pdf: image
	$(TEX) $(OBJECT)
	$(TEX) $(OBJECT)

tex: pdf

html: image
	htlatex.bat $(OBJECT) "html, imgdir:img/"
	mv *.png img/

clean:
	rm *.aux *.dvi *.log *.out *.pdf *~

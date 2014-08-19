HEADER =
SOURCE = preface-zh-cn.tex
OBJECT = preface-zh-cn
TEX = xelatex

.PHONY: all

all: pdf
tex: pdf

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

html: image
	htlatex.bat $(OBJECT) "html, imgdir:img/"
	mv *.png img/

clean:
	rm *.aux *.dvi *.log *.out *.pdf *~

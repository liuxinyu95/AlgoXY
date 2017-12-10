SOURCE = avl-proof
OBJECT = $(SOURCE)-en.pdf
OBJECT_CN = $(SOURCE)-zh-cn.pdf
TEX = xelatex

.PHONY: all

all: tex
tex: $(OBJECT) $(OBJECT_CN)

cpp:
	$(MAKE) cpp -C src

c:
	$(MAKE) c -C src

haskell:
	$(MAKE) haskell -C src

tool:
	$(MAKE) tool -C src

image:
	$(MAKE) -C ../../../datastruct/tree/AVL-tree/img

%.pdf: %.tex image
	$(TEX) $<
	$(TEX) $<

clean:
	rm -f *.aux *.dvi *.log *.out *.pdf *~

SRC = rbt-del
OBJ = $(SRC)-en.pdf
OBJ_CN = $(SRC)-zh-cn.pdf
TEX = xelatex

.PHONY: all

all: tex

cn: $(OBJ_CN)

en: $(OBJ)

tex: $(OBJ) $(OBJ_CN)

image:
	$(MAKE) -C ../../../datastruct/tree/red-black-tree/img

%.pdf: %.tex image
	$(TEX) $<
	$(TEX) $<

clean:
	rm -f *.4ct *.4tc *.aux *.css *.dvi *.html
	rm -f *.idv *.lg *.log *.out *.pdf *.tmp *.xref *.cache *.zip *~

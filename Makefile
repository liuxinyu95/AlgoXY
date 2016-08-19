BOOK = main-zh-cn
XELATEX = $(shell which xelatex > /dev/null)

ifdef XELATEX
LATEX = xelatex
DVIPDFM = echo
else
LATEX = latex
DVIPDFM = dvipdfmx
endif

SRC = common-zh-cn.tex main-zh-cn.tex
CHAPTERS = others/preface/preface-zh-cn \
datastruct/tree/binary-search-tree/bstree-zh-cn \
sorting/insertion-sort/isort-zh-cn \
datastruct/tree/red-black-tree/rbtree-zh-cn datastruct/tree/AVL-tree/avltree-zh-cn \
datastruct/tree/trie/trie-zh-cn \
datastruct/tree/suffix-tree/stree-zh-cn datastruct/tree/B-tree/btree-zh-cn \
datastruct/heap/binary-heap/bheap-zh-cn sorting/select-sort/ssort-zh-cn \
datastruct/heap/other-heaps/kheap-zh-cn \
datastruct/elementary/queue/queue-zh-cn \
datastruct/elementary/sequence/sequence-zh-cn \
sorting/dc-sort/dcsort-zh-cn \
search/search-zh-cn \
others/appendix/list/list-zh-cn
CHAPTER_OBJS = $(foreach file, $(CHAPTERS), $(file).pdf)
CHAPTER_SRCS = $(foreach file, $(CHAPTERS), $(file).tex)

all: $(BOOK).pdf

%.pdf : %.tex
	$(MAKE) -C $(@D) tex

image:
	$(MAKE) -C img

index:
	makeindex $(BOOK)

$(BOOK).pdf: image $(SRC) $(CHAPTER_SRC)
	$(LATEX) $(BOOK).tex
	makeindex $(BOOK).idx
	$(LATEX) $(BOOK).tex
	$(DVIPDFM) $(BOOK)

clean:
	rm -f *.aux *.toc *.lon *.lor *.lof *.ilg *.idx *.ind *.out *.log *.exa

distclean: clean
	rm -f *.pdf *.dvi *~

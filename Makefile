BOOK = main-en
XELATEX = $(shell which xelatex > /dev/null)

ifdef XELATEX
LATEX = xelatex
DVIPDFM = echo
else
LATEX = latex
DVIPDFM = dvipdfmx
endif

SRC = common-en.tex main-en.tex
CHAPTERS = others/preface/preface-en \
datastruct/tree/binary-search-tree/bstree-en \
sorting/insertion-sort/isort-en \
datastruct/tree/red-black-tree/rbtree-en datastruct/tree/AVL-tree/avltree-en \
datastruct/tree/trie/trie-en \
datastruct/tree/suffix-tree/stree-en datastruct/tree/B-tree/btree-en \
datastruct/heap/binary-heap/bheap-en sorting/select-sort/ssort-en \
datastruct/heap/other-heaps/kheap-en \
datastruct/elementary/queue/queue-en \
datastruct/elementary/sequence/sequence-en \
sorting/dc-sort/dcsort-en \
search/search-en \
others/appendix/list/list-en
CHAPTER_OBJS = $(foreach file, $(CHAPTERS), $(file).pdf)
CHAPTER_SRCS = $(foreach file, $(CHAPTERS), $(file).tex)

all: $(BOOK).pdf

%.pdf : %.tex
	$(MAKE) -C $(@D) tex

image:
	$(MAKE) -C img

index:
	makeindex $(BOOK)

$(BOOK).pdf: image $(SRC) $(CHAPTER_OBJS)
	$(LATEX) $(BOOK).tex
	makeindex $(BOOK).idx
	$(LATEX) $(BOOK).tex
	$(DVIPDFM) $(BOOK)

clean:
	rm -f *.aux *.toc *.lon *.lor *.lof *.ilg *.idx *.ind *.out *.log *.exa

distclean: clean
	rm -f *.pdf *.dvi *~

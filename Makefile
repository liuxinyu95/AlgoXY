BOOK = main-en
SRC = common-en.tex
CHAPTERS = others/preface

all: $(BOOK).pdf

index:
	makeindex $(BOOK)

$(BOOK).pdf: $(SRC)
	latex $(BOOK).tex
	makeindex $(BOOK).idx
	latex $(BOOK).tex
	dvipdfmx $(BOOK)

clean:
	rm -f *.aux *.toc *.lon *.lor *.lof *.ilg *.idx *.ind *.out *.log *.exa

distclean: clean
	rm -f *.pdf *.dvi *~


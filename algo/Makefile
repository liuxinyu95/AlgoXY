BOOK = main-en
SRC = common-en.tex

all: $(BOOK).pdf

$(BOOK).pdf: $(SRC)
	latex $(BOOK).tex
	makeindex -o $(BOOK).ind $(BOOK).idx #must have \makeindex in tex file
	latex $(BOOK).tex
	dvipdfmx $(BOOK).dvi

clean:
	rm -f *.aux *.toc *.lon *.lor *.lof *.ilg *.idx *.ind *.out *.log *.exa

distclean: clean
	rm -f *.pdf *.dvi *~

	


HEADER =
SOURCE = rbt.cc
CXX_FLAGS = -Wc++11-extensions

.PHONY: all

all: cpp haskell

cpp: $(HEADER)
	g++ $(CXX_FLAGS) $(SOURCE) -o rbt

haskell:
	ghc RBTree.hs -o rbt

clean:
	rm -f *.o main rbt *.pyc *.hi *.4ct *.4tc *.aux *.css *.dvi *.html
	rm -f *.idv *.lg *.log *.out *.pdf *.tmp *.xref *.cache *~

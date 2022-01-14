HEADER =
SOURCE = rbt.cc
CXX = clang++
CXX_FLAGS = -std=c++11 -stdlib=libc++ -Wall -Wno-c++98-compat


.PHONY: all

all: cpp haskell

cpp: $(HEADER)
	$(CXX) $(CXX_FLAGS) $(SOURCE) -o rbt

haskell:
	ghc RBTree.hs -o rbt

clean:
	rm -f *.o main rbt *.pyc *.hi *.4ct *.4tc *.aux *.css *.dvi *.html
	rm -f *.idv *.lg *.log *.out *.pdf *.tmp *.xref *.cache *~

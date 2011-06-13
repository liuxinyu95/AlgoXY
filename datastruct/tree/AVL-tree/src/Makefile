HEADER = 
SOURCE = xxx.cpp
#CXX_FLAGS = -I/usr/include/boost-1_33_1

.PHONY: all 

all: cpp haskell

cpp: $(HEADER)
	g++ $(CXX_FLAGS) $(SOURCE) -o main

haskell:
	ghc RBTree.hs -o rbt

clean:
	rm -f *.o main rbt *.pyc *.hi *.4ct *.4tc *.aux *.css *.dvi *.html 
	rm -f *.idv *.lg *.log *.out *.pdf *.tmp *.xref *.cache *~

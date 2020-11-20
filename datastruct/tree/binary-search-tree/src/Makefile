HEADER =
SOURCE = bstree.cpp
CXX_FLAGS = -std=c++17

.PHONY: all

all: cpp haskell

cpp: $(HEADER)
	g++ $(CXX_FLAGS) $(SOURCE) -o main

haskell:
	ghc BSTree.hs -o bst

clean:
	rm *.o *.hi *.pyc *~ main bst

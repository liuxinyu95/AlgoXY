HEADER = 
SOURCE = trie.cpp
#CXX_FLAGS = -I/usr/include/boost-1_33_1

.PHONY: all

all: cpp haskell

cpp: $(HEADER)
	g++ $(CXX_FLAGS) $(SOURCE) -o main

c:
	gcc trie.c -o triec

haskell:
	ghc IntTrie.hs -o inttrie
	ghc IntPatricia.hs -o intpatricia

clean:
	rm *.exe *.hi *~ *.o *.pyc
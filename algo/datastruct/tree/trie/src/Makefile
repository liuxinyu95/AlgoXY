HEADER = 
SOURCE = trie.cpp
HS_SOURCES = IntTrie.hs IntPatricia.hs Trie.hs Patricia.hs
HS_OBJECTS = inttrie intpatricia trie patricia
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
	ghc Trie.hs -o trie
	ghc Patricia.hs -o patricia

clean:
	rm *.exe *.hi *~ *.o *.pyc
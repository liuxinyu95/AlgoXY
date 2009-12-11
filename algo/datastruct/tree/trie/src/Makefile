HEADER = 
SOURCE = trie.cpp
HS_SOURCES = IntTrie.hs IntPatricia.hs Trie.hs Patricia.hs EDict.hs
HS_OBJECTS = $(HS_SOURCES:.hs=.o)
HS_TARGETS = inttrie intpatricia trie patricia
#CXX_FLAGS = -I/usr/include/boost-1_33_1

.PHONY: all

all: cpp haskell

cpp: $(HEADER)
	g++ $(CXX_FLAGS) $(SOURCE) -o main

c:
	gcc trie.c -o triec

haskell: 
	ghc --make IntTrie -o inttrie
	ghc --make IntPatricia -o intpatricia
	ghc --make Trie -o trie
	ghc --make Patricia -o patricia

clean:
	rm *.exe *.hi *~ *.o *.pyc
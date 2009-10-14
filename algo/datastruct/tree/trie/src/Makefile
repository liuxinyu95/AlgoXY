HEADER = 
SOURCE = trie.cpp
#CXX_FLAGS = -I/usr/include/boost-1_33_1

.PHONY: all

all: $(HEADER)
	g++ $(CXX_FLAGS) $(SOURCE) -o main

haskell:
	ghc IntTrie.hs -o inttrie
	ghc IntPatricia.hs -o intpatricia

clean:
	rm *.exe *.hi *~ *.o *.pyc
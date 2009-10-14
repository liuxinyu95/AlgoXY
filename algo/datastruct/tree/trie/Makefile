HEADER = 
SOURCE = trie.cpp
#CXX_FLAGS = -I/usr/include/boost-1_33_1

.PHONY: all

all: $(HEADER)
	g++ $(CXX_FLAGS) $(SOURCE) -o main

haskell:
	ghc IntTrie.hs -o inttrie
	ghc IntPatricia.hs -o intpatricia

image:
	convert img/radix-tree.png img/radix-tree.eps
	dot -Tps -o img/int-trie.ps img/int-trie.dot
	dot -Tps -o img/little-endian-trie.ps img/little-endian-trie.dot

html:
	htlatex.bat rbtree-en "html, imgdir:img/"
	mv *.png img/
HEADER = 
SOURCE = bstree.cpp
#CXX_FLAGS = -I/usr/include/boost-1_33_1

.PHONY: all

all: $(HEADER)
	g++ $(CXX_FLAGS) $(SOURCE) -o main

haskell:
	ghc BSTree.hs -o bst

image:
	convert img/tree.png img/tree.eps

html:
	htlatex.bat bstree-en "html, imgdir:img/"
	mv *.png img/
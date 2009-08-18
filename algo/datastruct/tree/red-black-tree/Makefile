HEADER = 
SOURCE = rbtree.cpp
#CXX_FLAGS = -I/usr/include/boost-1_33_1

.PHONY: all

all: $(HEADER)
	g++ $(CXX_FLAGS) $(SOURCE) -o main

haskell:
	ghc RBTree.hs -o rbt

image:
	dot -Tps -o img/unbalanced.ps img/unbalanced.dot
	convert img/insert-fix.png img/insert-fix.eps
	dot -Tps -o img/insert-haskell.ps img/insert-haskell.dot

html:
	htlatex.bat rbtree-en "html, imgdir:img/"
	mv *.png img/
HEADER = 
SOURCE = trie.cpp
#CXX_FLAGS = -I/usr/include/boost-1_33_1

.PHONY: all

all: $(HEADER)
	g++ $(CXX_FLAGS) $(SOURCE) -o main

haskell:
	ghc RBTree.hs -o rbt

image:
	dot -Tps -o img/unbalanced.ps img/unbalanced.dot
	dot -Tps -o img/insert-haskell.ps img/insert-haskell.dot
	dot -Tps -o img/clrs-fig.13.4.ps img/clrs-fig.13.4.dot
	dot -Tps -o img/python-insert.ps img/python-insert.dot
	dot -Tps -o img/db-fix.ps img/db-fix.dot
	convert img/rb-insert-clrs.png img/rb-insert-clrs.eps
	convert img/insert-fix.png img/insert-fix.eps
	convert img/del-case1.png img/del-case1.eps
	convert img/del-case2.png img/del-case2.eps
	convert img/del-case3.png img/del-case3.eps

html:
	htlatex.bat rbtree-en "html, imgdir:img/"
	mv *.png img/
HEADER = 
SOURCE = 

.PHONY: all

all: image

cpp:
	make cpp -C src

haskell:
	make haskell -C src

image:
	convert img/radix-tree.png img/radix-tree.eps
	dot -Tps -o img/int-trie.ps img/int-trie.dot
	dot -Tps -o img/little-endian-trie.ps img/little-endian-trie.dot
	dot -Tps -o img/little-endian-patricia.ps img/little-endian-patricia.dot

html:
	htlatex.bat rbtree-en "html, imgdir:img/"
	mv *.png img/

clean:
	rm *.aux *.dvi *.log *.out *.pdf *~
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
	dot -Tps -o img/big-endian-trie.ps img/big-endian-trie.dot
	dot -Tps -o img/little-endian-patricia.ps img/little-endian-patricia.dot
	dot -Tps -o img/int-patricia-insert-a.ps img/int-patricia-insert-a.dot
	dot -Tps -o img/int-patricia-insert-b.ps img/int-patricia-insert-b.dot
	dot -Tps -o img/int-patricia-insert-c.ps img/int-patricia-insert-c.dot
	dot -Tps -o img/int-patricia-insert-d.ps img/int-patricia-insert-d.dot
	dot -Tps -o img/int-patricia-haskell-insert.ps img/int-patricia-haskell-insert.dot

html:
	htlatex.bat trie-en "html, imgdir:img/"
	mv *.png img/

clean:
	rm *.aux *.dvi *.log *.out *.pdf *~
HEADER = 
SOURCE = rbtree-en.tex
OBJECT = rbtree-en
TEX = latex

.PHONY: all

all: pdf

cpp:
	make cpp -C src

haskell:
	make haskell -C src

image:
	#./create-image.sh
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
	htlatex.bat $(OBJECT) "html, imgdir:img/"
	mv *.png img/

clean:
	rm -f *.o main rbt *.pyc *.hi *.4ct *.4tc *.aux *.css *.dvi *.html 
	rm -f *.idv *.lg *.log *.out *.pdf *.tmp *.xref *.cache *.zip *~
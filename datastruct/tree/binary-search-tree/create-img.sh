#!/usr/bin/sh

CONV="../red-black-tree/src/rbt2dot.py"

#$CONV -o img/btexample.dot "((((. 2 .) 14 (. 8 .)) 4 ((. 1 .) 7 .)) 16 ((. 9 .) 10 (. 3 .)))"
#$CONV -o img/lvr.dot "((. L:N .) k (. R:N .))"
$CONV -o img/bst-1.dot "(((. 1 (. 2 .)) 3 .) 4 ((. 7 .) 8 (((. 9 .) 10 (. 14 .)) 16 .)))"

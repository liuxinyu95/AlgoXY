#/usr/bin/sh
./rbt2dot.py -o img/clrs-fig.13.4.dot "(((. 1:B .) 2:R ((. 5:R .) 7:B (. 8:R .))) 11:B (. 14:B (. 15:R .)))"
./rbt2dot.py -o img/python-insert.dot "(((. 1:B .) 2:R ((. 3:R .) 4:B .)) 5:B ((. 6:B .) 7:R ((. 8:R .) 9:B .)))"
./rbt2dot.py -o img/db-fix.dot "(((. 1:B .) 2:B .) 3:R ((. 4:B .) 5:B (. 6:B  .)))"
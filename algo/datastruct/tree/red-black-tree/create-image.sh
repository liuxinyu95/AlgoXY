#!/usr/bin/sh

# create-image.sh
# Copyright (C) 2010 Liu Xinyu (liuxinyu95@gmail.com)
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

./rbt2dot.py -o img/clrs-fig.13.4.dot "(((. 1:B .) 2:R ((. 5:R .) 7:B (. 8:R .))) 11:B (. 14:B (. 15:R .)))"
./rbt2dot.py -o img/python-insert.dot "(((. 1:B .) 2:R ((. 3:R .) 4:B .)) 5:B ((. 6:B .) 7:R ((. 8:R .) 9:B .)))"
./rbt2dot.py -o img/db-fix.dot "(((. 1:B .) 2:B .) 3:R ((. 4:B .) 5:B (. 6:B  .)))"
./rbt2dot.py -o img/case1-a.dot "((. a:BB .) x:C (((. b:N .) y:R (. c:N .)) z:B (. d:N .)))"
./rbt2dot.py -o img/case1-b.dot "((. a:BB .) x:C ((. b:N .) y:B ((. c:N .) z:R (. d:N .))))"
./rbt2dot.py -o img/case1-e.dot "(((. a:N .) x:B (. b:N .)) y:C ((. c:N .) z:B (. d:N .)))"
./rbt2dot.py -o img/case1-c.dot "(((. a:N .) x:B ((. b:N .) y:R (. c:N .))) z:C (. d:BB .))"
./rbt2dot.py -o img/case1-d.dot "((((. a:N .) x:R (. b:N .)) y:B (. c:N .)) z:C (. d:BB .))"
./rbt2dot.py -o img/case2-a.dot "((. a:BB .) x:C ((. b:B .) y:B (. c:B .)))"
./rbt2dot.py -o img/case2-a1.dot "((. a:BB .) x:BB ((. b:B .) y:R (. c:B .)))"
./rbt2dot.py -o img/case2-b.dot "(((. a:B .) x:B (. b:B .)) y:C (. c:BB .))"
./rbt2dot.py -o img/case2-b1.dot "(((. a:B .) x:R (. b:B .)) y:BB (. c:BB .))"
./rbt2dot.py -o img/case3-a.dot "((. a:BB .) x:B ((. b:B .) y:R (. c:B .)))"
./rbt2dot.py -o img/case3-a1.dot "(((. a:BB .) x:R (. b:B .)) y:B (. c:B .))"
./rbt2dot.py -o img/case3-b.dot "(((. a:B .) x:R (. b:B .)) y:B (. c:BB .))"
./rbt2dot.py -o img/case3-b1.dot "((. a:B .) x:B ((. b:B .) y:R (. c:BB .)))"

#!/usr/bin/sh

#
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
#

CONV="./src/rbt2dot.py"

#$CONV -o img/unbalanced-2.dot "(((((. 1 .) dot .) n2 .) n1 .) n .)"
#$CONV -o img/unbalanced-3.dot "(((((. 1 .) dot1 .) 2m .) 1m .) m (. m1 (. m2 (. dot2 (. n .)))))"
#$CONV -o img/unbalanced-zigzag.dot "((. 2 ((. 3 ((. 4 (. dot .)) 1n .)) n .)) 1 .)"
#$CONV -o img/clrs-fig.13.4.dot "(((. 1:B .) 2:R ((. 5:R .) 7:B (. 8:R .))) 11:B (. 14:B (. 15:R .)))"
#$CONV -o img/python-insert.dot "(((. 1:B .) 2:R ((. 3:R .) 4:B .)) 5:B ((. 6:B .) 7:R ((. 8:R .) 9:B .)))"
#$CONV -o img/db-fix.dot "(((. 1:B .) 2:B .) 3:R ((. 4:B .) 5:B (. 6:B  .)))"
#$CONV -o img/db-fix-1-nil-before.dot "(((. 1:B .) 2:B .) 3:R ((. NIL:BB .) 5:B (. 6:B  .)))"
#$CONV -o img/db-fix-1-nil-after.dot "(((. 1:B .) 2:B .) 3:R (. 5:B (. 6:B  .)))"
#$CONV -o img/db-fix-2-nil-before.dot "(((. NIL:BB .) 2:B .) 3:R ((. 4:B .) 5:B (. 6:B  .)))"
#$CONV -o img/db-fix-2-nil-after.dot "((. 2:BB .) 3:R ((. 4:B .) 5:B (. 6:B  .)))"
#$CONV -o img/case1-a.dot "((. a:BB .) x:C (((. b:N .) y:R (. c:N .)) z:B (. d:N .)))"
#$CONV -o img/case1-b.dot "((. a:BB .) x:C ((. b:N .) y:B ((. c:N .) z:R (. d:N .))))"
#$CONV -o img/case1-e.dot "(((. a:N .) x:B (. b:N .)) y:C ((. c:N .) z:B (. d:N .)))"
#$CONV -o img/case1-c.dot "(((. a:N .) x:B ((. b:N .) y:R (. c:N .))) z:C (. d:BB .))"
#$CONV -o img/case1-d.dot "((((. a:N .) x:R (. b:N .)) y:B (. c:N .)) z:C (. d:BB .))"
#$CONV -o img/case2-a.dot "((. a:BB .) x:C ((. b:B .) y:B (. c:B .)))"
#$CONV -o img/case2-a1.dot "((. a:B .) x:B ((. b:B .) y:R (. c:B .)))"
#$CONV -o img/case2-b.dot "(((. a:B .) x:B (. b:B .)) y:C (. c:BB .))"
#$CONV -o img/case2-b1.dot "(((. a:B .) x:R (. b:B .)) y:B (. c:B .))"
#$CONV -o img/case3-a.dot "((. a:BB .) x:B ((. b:B .) y:R (. c:B .)))"
#$CONV -o img/case3-a1.dot "(((. a:BB .) x:R (. b:B .)) y:B (. c:B .))"
#$CONV -o img/case3-b.dot "(((. a:B .) x:R (. b:B .)) y:B (. c:BB .))"
#$CONV -o img/case3-b1.dot "((. a:B .) x:B ((. b:B .) y:R (. c:BB .)))"
#$CONV -o img/rotate-r.dot "((. a .) X ((. b .) Y (. c .)))"
#$CONV -o img/rotate-l.dot "(((. a .) X (. b .)) Y (. c .))"
#$CONV -o img/rbt-example.dot "(((. 1:B (. 6:R .)) 8:R (. 11:B .)) 13:B ((. 15:B .) 17:R ((. 22:R .) 25:B (. 27:R .))))"
#$CONV -o img/insert-ll.dot "((((. A:N .) x:R (. B:N .)) y:R (. C:N .)) z:B (. D:N .))"
#$CONV -o img/insert-lr.dot "(((. A:N .) x:R ((. B:N .) y:R (. C:N .))) z:B (. D:N .))"
#$CONV -o img/insert-rr.dot "((. A:N .) x:B ((. B:N .) y:R ((. C:N .) z:R (. D:N .))))"
#$CONV -o img/insert-rl.dot "((. A:N .) x:B (((. B:N .) y:R (. C:N .)) z:R (. D:N .)))"
#$CONV -o img/insert-fixed.dot "(((. A:N .) x:B (. B:N .)) y:R ((. C:N .) z:B (. D:N .)))"
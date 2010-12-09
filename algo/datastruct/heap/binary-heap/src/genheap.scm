#|
    genheap.scm, Generic heap operations in Scheme.
    Copyright (C) 2010, Liu Xinyu (liuxinyu95@gmail.com)

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
|#

(define (find-min t)
  (elem t))

(define (delete-min t)
  (merge (left t) (right t)))

;; generic part

(define (from-list lst)
  (fold-left insert '() lst))

(define (heap-sort lst)
  (define (hsort t)
    (if (null? t) '() (cons (find-min t) (hsort (delete-min t)))))
  (hsort (from-list lst)))

;; test
(define (test-from-list)
  (from-list '(16 14 10 8 7 9 3 2 4 1)))

(define (test-sort)
  (heap-sort '(16 14 10 8 7 9 3 2 4 1)))
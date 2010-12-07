#|
    leftist.scm, Leftist heap in Scheme.
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

;; for pretty print purpose, the leftist tree is arranged as
;; (left rank element right)

;; access functions

(define (left t)
  (if (null? t) '() (car t)))

(define (rank t)
  (if (null? t) 0 (cadr t)))

(define (elem t)
  (if (null? t) '() (caddr t)))

(define (right t)
  (if (null? t) '() (cadddr t)))

(define (make-tree l s x r) ;; l: left, s: rank, x: elem, r: right
  (list l s x r))

(define (merge t1 t2)
  (define (make-node x a b)
    (if (< (rank a) (rank b))
	(make-tree b (+ (rank a) 1) x a)
	(make-tree a (+ (rank b) 1) x b)))
  (cond ((null? t1) t2)
	((null? t2) t1)
	((< (elem t1) (elem t2)) (make-node (elem t1) (left t1) (merge (right t1) t2)))
	(else (make-node (elem t2) (left t2) (merge t1 (right t2))))))

(define (insert t x)
  (merge (make-tree '() 1 x '()) t))

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
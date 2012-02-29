#|
 pairing-heap.scm Pairing heap in Scheme/Lisp
 Copyright (C) 2012 Liu Xinyu (liuxinyu95@gmail.com)
 
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

;; Definition
(define (make-heap root trees)
  (list root trees))

;; Auxiliary functions
(define (root h) (car h))

(define (trees h) (cadr h))

;; merge, O(1) time
(define (merge h1 h2)
  (cond ((null? h1) h2)
	((null? h2) h1)
	((< (root h1) (root h2))
	 (make-heap (root h1) (cons h2 (trees h1))))
	(else (make-heap (root h2) (cons h1 (trees h2))))))

;; insertion
(define (insert h x)
  (merge h (make-heap x '())))

;; top
(define find-min root)

;; pop
(define (del-min h)
  (define (merge-pairs ts)
    (cond ((null? ts) '())
	  ((null? (cdr ts)) (car ts))
	  (else (merge (merge (car ts) (cadr ts))
		       (merge-pairs (cddr ts))))))
  (merge-pairs (trees h)))

;; Helper function
(define (list->heap lst)
  (fold-left insert '() lst))

(define (heap-sort lst)
  (define (hsort h)
    (if (null? h) '()
	(cons (find-min h) (hsort (del-min h)))))
  (hsort (list->heap lst)))

;; testing
(define (rand-seq n m)
  (map (lambda (x) (random m)) (iota n)))

;; test n random test cases
(define (test n)
  (define (assert exp)
    (if (not exp) (error "fail" exp)))
  (define (test-sort i)
    (let* ((m (random 1000))
	   (xs (rand-seq m 1000)))
      (assert (equal? (heap-sort xs) (sort xs <)))))
  (for-each test-sort (iota n)))
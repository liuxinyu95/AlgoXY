#|
 binomial-heap.scm Binomial Heap in Scheme/Lisp
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
(define (make-node rank root children)
  (list rank root children))

;; Auxiliary functions
(define (rank node) (car node))

(define (root node) (cadr node))

(define (children node) (caddr node))

;; linking
;;  assume the ranks are same
(define (link t1 t2)
  (if (< (root t1) (root t2))
      (make-node (+ (rank t1) 1) (root t1) (cons t2 (children t1)))
      (make-node (+ (rank t2) 1) (root t2) (cons t1 (children t2)))))

;; assume (rank t) is either less or equal to the first tree in h
(define (insert-tr h t)
  (if (and (not (null? h)) (= (rank t) (rank (car h))))
      (insert-tr (cdr h) (link t (car h)))
      (cons t h)))

;; insertion
(define (insert h x)
  (insert-tr h (make-node 0 x '())))

;; merge
(define (merge h1 h2)
  (cond ((null? h1) h2)
	((null? h2) h1)
	((< (rank (car h1)) (rank (car h2))) 
	 (cons (car h1) (merge (cdr h1) h2)))
	((> (rank (car h1)) (rank (car h2)))
	 (cons (car h2) (merge h1 (cdr h2))))
	(else (insert-tr (merge (cdr h1) (cdr h2)) (link (car h1) (car h2))))))

;; Auxiliary function for deleting
(define (extract-min h)
  (if (null? (cdr h)) h 
      (let* ((r (extract-min (cdr h)))
	     (t (car r))
	     (ts (cdr r)))
	(if (< (root (car h)) (root t)) h
	    (cons t (cons (car h) ts))))))

;; top
(define (find-min h)
  (root (car (extract-min h))))

;; pop
(define (del-min h)
  (let ((r (extract-min h)))
    (merge (reverse (children (car r))) (cdr r))))

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
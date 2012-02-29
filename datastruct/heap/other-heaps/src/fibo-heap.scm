#|
 fibo-heap.scm Fibonacci heap in Scheme/Lisp
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

(define (make-heap size min-tree trees)
  (list size min-tree trees))

;; Auxiliary functions
(define (rank node) (car node))

(define (root node) (cadr node))

(define (children node) (caddr node))

(define (size h) (car h))

(define (min-tree h) (cadr h))

(define (trees h) (caddr h))

(define (singleton x)
  (make-heap 1 (make-node 1 x '()) '()))

;; linking
;;  assume the ranks are same
(define (link t1 t2)
  (if (< (root t1) (root t2))
      (make-node (+ (rank t1) 1) (root t1) (cons t2 (children t1)))
      (make-node (+ (rank t2) 1) (root t2) (cons t1 (children t2)))))

;; insertion
(define (insert h x)
  (merge h (singleton x)))

;; merge
;;  Different fromt Binomial heap, We dont' consolidate the sub trees 
;;  immediately, but delay it later when perfroming del-min (pop).
(define (merge h1 h2)
  (cond ((null? h1) h2)
	((null? h2) h1)
	((< (root (min-tree h1)) (root (min-tree h2)))
	 (make-heap (+ (size h1) (size h2)) 
		    (min-tree h1)
		    (cons (min-tree h2) (append (trees h2) (trees h1)))))
	(else
	 (make-heap (+ (size h1) (size h2))
		    (min-tree h2)
		    (cons (min-tree h1) (append (trees h1) (trees h2)))))))

;; top
(define (find-min h)
  (root (min-tree h)))

;; Consolidate unordered trees by melding all trees in same rank
;;  O(lg N) time
(define (consolidate lst)
  (define (meld ts t)
    (if (null? ts) (list t)
	(cond ((= (rank t) (rank (car ts)))
	       (meld (cdr ts) (link t (car ts))))
	      ((< (rank t) (rank (car ts)))
	       (cons t ts))
	      (else (cons (car ts) (meld (cdr ts) t))))))
  (fold-left meld '() lst))

;; Find the tree which contains the minimum element
;;   O(lg N) time
(define (extract-min lst)
  (if (null? (cdr lst)) lst 
      (let* ((r (extract-min (cdr lst)))
	     (t (car r))
	     (ts (cdr r)))
	(if (< (root (car lst)) (root t)) lst
	    (cons t (cons (car lst) ts))))))

;; pop
(define (del-min h)
  (if (= (size h) 1) '()
      (let ((r (extract-min (consolidate (append (children (min-tree h)) 
						 (trees h))))))
	(make-heap (- (size h) 1) (car r) (cdr r)))))

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
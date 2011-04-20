#|
    intpatricia.scm, Integer based Patricia tree.
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

;; Integer Patricia

(load "trieutil.scm")

;; Definition

(define (make-leaf k v) ;; key and value
  (list k v))

(define (make-branch p m l r) ;; prefix, mask, left and right
  (list p m l r))

;; Helpers
(define (leaf? t)
  (= (length t) 2))

(define (branch? t)
  (= (length t) 4))

(define (key t)
  (if (leaf? t) (car t) '()))

(define (value t)
  (if (leaf? t) (cadr t) '()))

(define (prefix t)
  (if (branch? t) (car t) '()))

(define (mask t)
  (if (branch? t) (cadr t) '()))

(define (left t)
  (if (branch? t) (caddr t) '()))

(define (right t)
  (if (branch? t) (cadddr t) '()))

;; Utils

(define (mask-bit x m)
  (fix:and x (fix:not (- m 1))))

(define (zero-bit? x m)
  (= (fix:and x (fix:lsh m -1)) 0))

(define (lcp x y) ;; get the longest common prefix
  (define (count-mask z)
    (if (= z 0) 1 (* 2 (count-mask (fix:lsh z -1)))))
  (let* ((m (count-mask (fix:xor x y)))
	 (p (mask-bit x m)))
    (cons p m)))

(define (match? k p m)
  (= (mask-bit k m) p))
	
(define (branch p1 t1 p2 t2) ;; pi: prefix i, ti: Patricia i
  (let* ((pm (lcp p1 p2)) 
	 (p (car pm)) 
	 (m (cdr pm)))
    (if (zero-bit? p1 m)
	(make-branch p m t1 t2)
	(make-branch p m t2 t1))))

;; Insertion
(define (insert t k x) ;; t: patrica, k: key, x: value
  (cond ((null? t) (make-leaf k x))
	((leaf? t) (if (= (key t) k) 
		       (make-leaf k x) ;; overwrite
		       (branch k (make-leaf k x) (key t) t)))
	((branch? t) (if (match? k (prefix t) (mask t))
			 (if (zero-bit? k (mask t))
			     (make-branch (prefix t) 
					  (mask t) 
					  (insert (left t) k x) 
					  (right t))
			     (make-branch (prefix t)
					  (mask t)
					  (left t)
					  (insert (right t) k x)))
			 (branch k (make-leaf k x) (prefix t) t)))))

;; Lookup
(define (lookup t k)
  (cond ((null? t) '())
	((leaf? t) (if (= (key t) k) (value t) '()))
	((branch? t) (if (match? k (prefix t) (mask t))
			 (if (zero-bit? k (mask t))
			     (lookup (left t) k)
			     (lookup (right t) k))
			 '()))))

(define (test-int-patricia)
  (define t (list->trie (list '(1 "x") '(4 "y") '(5 "z"))))
  (display t) (newline)
  (display "lookup 4: ") (display (lookup t 4)) (newline)
  (display "lookup 0: ") (display (lookup t 0)) (newline))
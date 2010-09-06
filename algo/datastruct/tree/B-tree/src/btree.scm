#|
    btree.scm, B-tree in Scheme.
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

;; example (("A" "B") "C" ("D" "E"))

(define (keys tr)
  (if (null? tr) 
      '()
      (if (list? (car tr))
	  (keys (cdr tr))
	  (cons (car tr) (keys (cdr tr))))))

(define (children tr)
  (if (null? tr)
      '()
      (if (list? (car tr))
	  (cons (car tr) (children (cdr tr)))
	  (children (cdr tr)))))

(define (leaf? tr)
  (or (null? tr)
      (not (list? (car tr)))))

(define (full? tr t) ;; t: minimum degree
  (> (length (keys tr)) 
     (- (* 2 t) 1)))

(define (low? tr t) ;; t: minimum degree
  (< (length (keys tr)) 
     (- t 1)))

(define (split tr t)
  (if (leaf? tr)
      (list (list-head tr (- t 1))
	    (list-ref tr (- t 1))
	    (list-tail tr t))
      (list (list-head tr (- (* t 2) 1)) 
	    (list-ref tr (- (* t 2) 1))
	    (list-tail tr (* t 2)))))

(define (un-split lst)
  (let ((c1 (car lst))
	(k (cadr lst))
	(c2 (caddr lst)))
    (append c1 (list k) c2)))

(define (cons-pair c k lst)
  (cons c (cons k lst)))


;; general tools
(define (rest lst k)
  (list-tail lst (- (length lst) k)))

(define (except-rest lst k)
  (list-head lst (- (length lst) k)))

(define (first lst)
  (if (null? lst) '() (car lst)))

(define (last lst)
  (if (null? lst) '() (car (last-pair lst))))

(define (inits lst)
  (if (null? lst) '() (except-last-pair lst)))

;; (c1 k1 c2 k2 ... c[i-1] k[i-1] ci ki ... cn kn c[n+1]) 
;; k[i-1] < k < k[i]
;; return:
;;  left <- (c1 k1 c2 k2 ... c[i-1] k[i-1])
;;  c <- ci
;;  right <- (ki c[i+1] ... cn kn c[n+1])
(define (partition-by tr x)
  (define (part-by pred tr x)
    (if (= (length tr) 1)
	(list '() (car tr) '())
	(if (pred (cadr tr) x)
	    (let* ((res (part-by pred (cddr tr) x))
		   (left (car res))
		   (c (cadr res))
		   (right (caddr res)))
	      (list (cons-pair (car tr) (cadr tr) left) c right))
	    (list '() (car tr) (cdr tr)))))
  (if (string? x)
      (part-by string<? tr x)
      (part-by < tr x)))

;; auxiliary function
(define (ordered-insert lst x)
  (define (insert-by less-p lst x)
    (if (null? lst)
	(list x)
	(if (less-p x (car lst))
	    (cons x lst)
	    (cons (car lst) (insert-by less-p (cdr lst) x)))))
  (if (string? x)
      (insert-by string<? lst x)
      (insert-by < lst x)))

;; insert a key into btree
(define (btree-insert tr x t)
  (define (ins tr x)
    (if (leaf? tr)
	(ordered-insert tr x) ;;leaf
	(let* ((res (partition-by tr x))
	       (left (car res))
	       (c (cadr res))
	       (right (caddr res)))
	  (make-btree left (ins c x) right t))))
  (fix-root (ins tr x) t))

;; delete a key from btree
(define (btree-delete tr x t)
  (define (del tr x)
    (if (leaf? tr)
	(delete x tr)
	(let* ((res (partition-by tr x))
	       (left (car res))
	       (c (cadr res))
	       (right (caddr res)))
	  (if (equal? (first right) x)
	      (merge-btree (append left (list c)) (cdr right) t)
	      (make-btree left (del c x) right t)))))
  (fix-root (del tr x) t))
			   
;; make a btree from left, c, and right
;; where
;;  left = (c1 k1 c2 k2 ... ci ki)
;;     c = c[i+1]
;;  right= (k[i+1] ... cn kn c[n+1])
(define (make-btree l c r t)
  (cond ((full? c t) (fix-full l c r t))
	((low? c t) (fix-low l c r t))
	(else (append l (cons c r)))))

;; merge two btree node into one.
(define (merge-btree tr1 tr2 t)
  (if (leaf? tr1)
      (append tr1 tr2)
      (make-btree (inits tr1)
		  (merge-btree (last tr1) (car tr2) t)
		  (cdr tr2)
		  t)))

;; fixing

(define (fix-full l c r t)
  (append l (split c t) r))

(define (fix-low l c r t)
  (cond ((not (null? (keys l)))
	 (make-btree (except-rest l 2)
		     (un-split (append (rest l 2) (list c)))
		     r t))
	((not (null? (keys r)))
	 (make-btree l
		     (un-split (cons c (list-head r 2)))
		     (list-tail r 2) t))
	(else c)))

(define (fix-root tr t)
  (cond ((null? tr) '()) ;; empty tree
	((full? tr t) (split tr t))
	((null? (keys tr)) (car tr)) ;; shrink height
	(else tr)))

(define (btree-search tr x)
  ;; find the smallest index where keys[i]>= x
  (define (find-index tr x)
    (let ((pred (if (string? x) string>=? >=)))
      (if (null? tr)
	  0
	  (if (and (not (list? (car tr))) (pred (car tr) x))
	      0
	      (+ 1 (find-index (cdr tr) x))))))
  (let ((i (find-index tr x)))
    (if (and (< i (length tr)) (equal? x (list-ref tr i)))
	(cons tr i)
	(if (leaf? tr) #f (btree-search (list-ref tr (- i 1)) x)))))

;; helper

(define (list->btree lst t)
  (fold-left (lambda (tr x) (btree-insert tr x t)) '() lst))

(define (str->slist s)
  (if (string-null? s)
      '()
      (cons (string-head s 1) (str->slist (string-tail s 1)))))

;; testing
(define (test-insert)
  (list->btree (str->slist "GMPXACDEJKNORSTUVYZBFHIQW") 3))

(define (test-delete)
  (define (del-and-show tr x)
    (let ((r (btree-delete tr x 3)))
      (begin (display r) (display "\n") r)))
  (fold-left del-and-show
	     (list->btree (str->slist "GMPXACDEJKNORSTUVYZBFHIQW") 3)
	     (str->slist "EGAMU")))

(define (test-search)
  (define (search-and-show tr x)
    (if (btree-search tr x)
	(display (list "found " x))
	(display (list "not found " x))))
  (let* ((lst (str->slist "GMPXACDEJKNORSTUVYZBFHIQW"))
	 (tr (list->btree lst 3)))
    (map (lambda (x) (search-and-show tr x)) (cons "L" lst))))
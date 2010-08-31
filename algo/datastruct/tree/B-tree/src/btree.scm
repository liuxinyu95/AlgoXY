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

(define (full? tr t) ;; t: minimum degree
  (> (length (keys tr)) 
     (- (* 2 t) 1)))

(define (low? tr t) ;; t: minimum degree
  (< (length (keys tr)) 
     (- t 1)))

(define (split tr t)
  (if (null? (children tr))
      (list (list-head tr (- t 1))
	    (list-ref tr (- t 1))
	    (list-tail tr t))
      (list (list-head tr (- (* t 2) 1)) 
	    (list-ref tr (- (* t 2) 1))
	    (list-tail tr (* t 2)))))

(define (cons-pair c k lst)
  (cons c (cons k lst)))

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
(define (orderred-insert lst x)
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
(define (insert tr x t)
  (define (ins tr x)
    (if (null? (children tr))
	(orderred-insert (keys tr) x) ;;leaf
	(let* ((res (partition-by tr x))
	       (left (car res))
	       (c (cadr res))
	       (right (caddr res)))
	  (make-btree left (ins c x) right t))))
  (fix-root (ins tr x) t))

(define (make-btree l c r t)
  (cond ((full? c t) (fix-full l c r t))
	;;(low? c t) (fix-low l c r t)
	(else (append l (cons c r)))))

;; fixing

(define (fix-full l c r t)
  (append l (split c t) r))

(define (fix-root tr t)
  (cond ((full? tr t) (split tr t))
	(else tr)))

;; helper

(define (list->btree lst t)
  (fold-left (lambda (tr x) (insert tr x t)) '() lst))

(define (str->slist s)
  (if (string-null? s)
      '()
      (cons (string-head s 1) (str->slist (string-tail s 1)))))

;; testing
(define (test-insert)
  (list->btree (str->slist "GMPXACDEJKNORSTUVYZBFHIQW") 3))
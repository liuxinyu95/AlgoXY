#|
    strstr.scm, String manipulation over suffix tree
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

(load "stree.scm")

(define TERM1 "$")
(define TERM2 "#")

(define (edge t)
  (car t))

(define (children t)
  (cdr t))

(define (leaf? t)
  (null? (children t)))

;;find the ocurrence of a pattern in a string
(define (lookup-pattern t ptn)
  (define (number-of-branches node)
    (if (null? node) 1 (length node)))
  (if (null? t) 0
      (let ((s (edge (car t)))
	    (tr (children (car t))))
	(cond ((string-prefix? ptn s)(number-of-branches tr))
	      ((string-prefix? s ptn)
	       (lookup-pattern tr (string-tail ptn (string-length s))))
	      (else lookup-pattern (cdr t) ptn)))))

(define (compare-on func)
  (lambda (x y) 
    (cond ((< (func x) (func y)) 'lt)
	  ((> (func x) (func y)) 'gt)
	  (else 'eq))))

(define (max-by comp lst)
  (define (update-max xs x)
    (case (comp (car xs) x)
      ('lt (list x))
      ('gt xs)
      (else (cons x xs))))
  (if (null? lst)
      '()
      (fold-left update-max (list (car lst)) (cdr lst))))

;; longest repeated substring searching.
(define (lrs t)
  (define (find lst)
    (if (null? lst)
	'("")
	(let ((s (edge (car lst)))
	      (tr (children (car lst))))
	  (max-by (compare-on string-length) 
		  (append
		   (map (lambda (x) (string-append s x)) (lrs tr))
		   (find (cdr lst)))))))
  (if (leaf? t)
      '("")
      (find (filter (lambda (x) (not (leaf? x))) t))))

(define (longest-repeated-substring s)
  (lrs (suffix-tree (string-append s TERM1))))

;; longest common substring searching

;; Search in the tree
(define (search-stree t match)
  (define (find lst)
    (if (null? lst)
	'()
	(let ((s (edge (car lst)))
	      (tr (children (car lst))))
	  (max-by (compare-on string-length) 
		  (if (match tr)
		      (cons s (find (cdr lst)))
		      (append
		       (map (lambda (x) (string-append s x)) (search-stree tr match))
		       (find (cdr lst))))))))
  (if (leaf? t)
      '()
      (find (filter (lambda (x) (not (leaf? x))) t))))

(define (xor x y)
  (not (eq? x y)))

(define (longest-common-substring s1 s2)
  (define (match-fork t)
    (and (eq? 2 (length t)) 
	 (and (leaf? (car t)) (leaf? (cadr t)))
	 (xor (substring? TERM2 (edge (car t)))
	     (substring? TERM2 (edge (cadr t))))))
  (search-stree (suffix-tree (string-append s1 TERM2 s2 TERM1)) match-fork))

(define (longest-palindrome s)
  (longest-common-substring (string-append s TERM2) 
			    (string-append (reverse-string s) TERM1)))

;; test
(define (test-main)
  (let ((fs (list longest-repeated-substring longest-palindrome)) 
	(ss '("mississippi" "banana" "cacao" "foofooxbarbar")))
    (map (lambda (f) (map f ss)) fs)))

(define (test-lcs)
  (longest-common-substring "xbaby" "ababa"))

(define (test-pattern)
  (define (test-ptn t s)
    (cons (string-append "find pattern " s " in banana" )
	  (lookup-pattern t s)))
  (let ((t (suffix-tree "banana")))
    (map (lambda (x) (test-ptn t x)) '("ana" "an" "anan" "nana" "anana"))))
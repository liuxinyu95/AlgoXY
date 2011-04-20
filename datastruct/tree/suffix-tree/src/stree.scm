#|
    stree.scm, suffix tree/Trie in Scheme.
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

;; Utilities

(define (string-car s)
  (if (string=? s "")
      ""
      (string-head s 1)))

(define (string-cdr s)
  (if (string=? s "")
      ""
      (string-tail s 1)))

(define (tails s)
  (if (string-null? s)
      '("")
      (cons s (tails (string-tail s 1)))))

;; (edge-trie '("an" "another" "and"))
;;  ==> ("a" "n" "nother" "nd")
(define (edge-trie ss)
  (cons (string-car (car ss)) (map string-cdr ss)))

;; test if a list of strings has common prefix
;; (prefix '("an" "another" "and")) ==> true
;; (prefix '("" "other" "d")) ==> false
(define (prefix? ss)
  (if (null? ss)
      '()
      (let ((c (string-car (car ss))))
	(null? (filter (lambda (s) (not (string=? c (string-car s))))
		       (cdr ss))))))

;; (edge-tree '("an" "another" "and"))
;;   ==> ("an" "" "other" "d")
(define (edge-tree ss)
  (cond ((= 1 (length ss)) (cons (car ss) '()))
	((prefix? ss)
	 (let* ((res (edge-tree (map string-cdr ss)))
		(prefix (car res))
		(ss1 (cdr res)))
	   (cons (string-append (string-car (car ss)) prefix) ss1)))
	(else (cons "" ss))))

;; overwite the partition if not supprt SRFI 1
;; (partition (> 5) '(1 6 2 7 3 9 0))
;;   ==>((6 7 9) 1 2 3 0)
(define (partition pred lst)
  (if (null? lst)
      (cons '() '())
      (let ((res (partition pred (cdr lst))))
	(if (pred (car lst))
	    (cons (cons (car lst) (car res)) (cdr res))
	    (cons (car res) (cons (car lst) (cdr res)))))))

;; group a list of strings based on first char
;; ss shouldn't contains "" string, so filter should be done first.
;; (groups '("an" "another" "bool" "and" "bar" "c"))
;;  ==> (("an" "another" "and") ("bool" "bar") ("c"))
(define (groups ss)
  (if (null? ss) 
      '()
      (let* ((c (string-car (car ss)))
	     (res (partition (lambda (x) (string=? c (string-car x))) (cdr ss))))
	(append (list (cons (car ss) (car res)))
		(groups (cdr res))))))

(define (remove-empty ss)
  (filter (lambda (s) (not (string=? "" s))) ss))
      
(define (make-tree edge ss)
  (define (bld-group grp)
    (let* ((res (edge grp))
	   (prefix (car res))
	   (ss1 (cdr res)))
      (cons prefix (make-tree edge ss1))))
  (let ((ss1 (remove-empty ss)))
    (if (null? ss1) '()
	(map bld-group (groups ss1)))))

(define (suffix-tree s)
  (make-tree edge-tree (tails s)))

(define (suffix-trie s)
  (make-tree edge-trie (tails s)))

(define (test-suffix-tree)
  (map suffix-tree '("cacao" "mississippi" "bananas")))
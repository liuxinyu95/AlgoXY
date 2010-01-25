#|
    patricia.scm, Alphabetic Patricia tree.
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

;; Alphabetic Patrica

(load "trie.scm")

;; Definition, we reused trie defintion
(define (make-leaf x)
  (make-trie x '()))

(define (match? x y)
  (and (not (or (string-null? x) (string-null? y)))
       (string=? (string-car x) (string-car y))))

(define (branch k1 x k2 t2) ;; returns (key tree)
  (let* ((k (lcp k1 k2))
	 (k1-new (string-tail k1 (string-length k)))
	 (k2-new (string-tail k2 (string-length k))))
    (cond ((string=? k1 k) ;; e.g. insert "an" into "another"
	   (make-child k (make-trie x (list (make-child k2-new t2)))))
	  ((string=? k2 k) ;; e.g. insert "another" into "an"
	   (make-child k (insert t2 k1-new x)))
	  (else (make-child k (make-trie 
			       '() 
			       (list (make-child k1-new (make-leaf x))
				     (make-child k2-new t2))))))))

(define (lcp x y)
  (let ((len (string-match-forward x y)))
    (string-head x len)))
      
;; Insertion
(define (insert t k x)
  (define (ins lst k x) ;; lst: [(key patrica)]
    (if (null? lst) (list (make-child k (make-leaf x)))
	(cond ((string=? (key (car lst)) k)
	       (cons (make-child k (make-trie x (children (tree (car lst)))))
		     (cdr lst)))
	      ((match? (key (car lst)) k)
	       (cons (branch k x (key (car lst)) (tree (car lst)))
		     (cdr lst)))
	      (else (cons (car lst) (ins (cdr lst) k x))))))
  (make-trie (value t) (ins (children t) k x)))

;; Lookup

(define (lookup t k)
  (define (find lst k) ;; lst, [(k patrica)]
    (if (null? lst) '()
	(cond ((string=? (key (car lst)) k) (value (tree (car lst))))
	      ((string-prefix? (key (car lst)) k)
	       (lookup (tree (car lst)) 
		     (string-tail k (string-length (key (car lst))))))
	      (else (find (cdr lst) k)))))
  (find (children t) k))

(define (test-patricia)
  (define t (list->trie (list '("a" 1) '("an" 2) '("another" 7) '("boy" 3) '("bool" 4) '("zoo" 3))))
  (define t2 (list->trie (list '("zoo" 3) '("bool" 4) '("boy" 3) '("another" 7) '("an" 2) '("a" 1))))
  (display (trie->string t)) (newline)
  (display (trie->string t2)) (newline)
  (display "lookup another: ") (display (lookup t "another")) (newline)
  (display "lookup bo: ") (display (lookup t "bo")) (newline)
  (display "lookup boy: ") (display (lookup t "boy")) (newline)
  (display "lookup by: ") (display (lookup t "by")) (newline)
  (display "lookup boolean: ") (display (lookup t "boolean")) (newline))

;; Find all candidates
(define (find t k)
  (define (find-child lst k)
    (if (null? lst) '()
	(cond ((string=? (key (car lst)) k) 
	       (map-string-append k (enumerate (tree (car lst)))))
	      ((string-prefix? (key (car lst)) k) 
	       (let ((k-new (string-tail k (string-length (key (car lst))))))
		 (map-string-append (key (car lst)) (find (tree (car lst)) k-new))))
	      ((string-prefix? k (key (car lst))) (enumerate (tree (car lst))))
	      (else (find-child (cdr lst) k)))))
  (if (string-null? k)
      (enumerate t)
      (find-child (children t) k)))

(define (test-patricia-find)
  (define t (list->trie dict))
  (display "find a*: ") (display (find t "a")) (newline)
  (display "find ab*: ") (display (find t "ab")) (newline))

(define (str->t9 s)
  (define (unmap-t9 c)
    (car (find-matching-item map-T9 (lambda (x) (substring? c (cadr x))))))
  (if (string-null? s) ""
      (string-append (unmap-t9 (string-car s)) (str->t9 (string-cdr s)))))

(define (find-T9 t k)
  (define (accumulate-find lst child)
    (append (map-string-append (key child) (find-T9 (tree child) (string- k (key child))))
	    lst))
  (define (lookup-child lst k)
    (filter (lambda (child) (string-prefix? (str->t9 (key child)) k)) lst))
  (if (string-null? k) (list (cons "" (value t)))
      (fold-left accumulate-find '() (lookup-child (children t) k))))

(define (test-patricia-T9)
  (define t (list->trie dict-T9))
  (display (trie->string t)) (newline)
  (display "find 4: ") (display (find-T9 t "4")) (newline)
  (display "find 46: ") (display (find-T9 t "46")) (newline)
  (display "find 466: ") (display (find-T9 t "466")) (newline)
  (display "find 4663: ") (display (find-T9 t "4663")) (newline)
  (display "find 2: ") (display (find-T9 t "2")) (newline)
  (display "find 22: ") (display (find-T9 t "22")) (newline))

(define (test-all)
  (test-patricia)
  (test-patricia-find)
  (test-patricia-T9))
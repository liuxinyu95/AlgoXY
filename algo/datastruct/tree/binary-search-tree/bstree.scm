#|
 bstree.scm
 Copyright (C) 2010 Liu Xinyu (liuxinyu95@gmail.com)
 
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
(define (make-tree left key right)
  (list left key right))

;; Helper functions
(define (key tree) (cadr tree))
(define (left tree) 
  (if (null? tree) '() (car tree)))
(define (right tree) 
  (if (null? tree) '() (caddr tree)))

;; In order tree walk
(define (in-order-walk tree f)
  (if (null? tree) 
      tree
      (make-tree (in-order-walk (left tree) f)
		 (f (key tree))
		 (in-order-walk (right tree) f))))

;; Querying
(define (tree-search tree x)
  (cond ((null? tree) tree)
	((equal? x (key tree)) tree)
	((< x (key tree)) (tree-search (left tree) x))
	(else (tree-search (right tree) x))))

(define (tree-min tree)
  (if (null? (left tree))
      tree
      (tree-min (left tree))))

(define (tree-max tree)
  (if (null? (right tree))
      tree
      (tree-max (right tree))))

;; low efficiency functions
(define (parent tree x) ; x is a node, not a value
  (define (parent? t v)
    (if (null? t) t (equal? (key t) v)))
  (cond ((null? tree) tree)
	((< (key x) (key tree)) (if (parent? (left tree) (key x))
				    tree
				    (parent (left tree) x)))
	((> (key x) (key tree)) (if (parent? (right tree) (key x))
				    tree
				    (parent (right tree) x)))
	(else '())))

(define (find-ancestor tree x f) ; x is a node, not a value
  (define (check-parent t p v f)
    (if (f (key p) v)
	p
	(check-parent t (parent t p) v f)))
  (check-parent tree (parent tree x) (key x) f))

(define (succ tree x) ;x is a node, not a value
  (if (null? (right x))
      (find-ancestor tree x >)
      (tree-min (right x))))

(define (pred tree x)
  (if (null? (left x))
      (find-ancestor tree x <)
      (tree-max (left x))))
      
;; Insertion and deletion

(define (tree-insert tree x)
  (cond ((null? tree) (list '() x '()))
	((< x (key tree))
	 (make-tree (tree-insert (left tree) x)
		    (key tree)
		    (right tree)))
	((> x (key tree))
	 (make-tree (left tree)
		    (key tree)
		    (tree-insert (right tree) x)))))

(define (tree-delete tree v)
  (cond ((null? tree) tree)
	((< v (key tree)) (make-tree (tree-delete (left tree) v)
				     (key tree)
				     (right tree)))
	((> v (key tree)) (make-tree (left tree)
				     (key tree)
				     (tree-delete (right tree) v)))
	((null? (left tree)) (right tree))
	((null? (right tree)) (left tree))
	(else (let ((newkey (key (tree-min (right tree)))))
		(make-tree (left tree)
			   newkey
			   (tree-delete (right tree) newkey))))))

;; Helpers

(define (list->tree lst)
  (fold-left tree-insert '() lst))

;; testing

(define t1 (list->tree '(15 6 18 3 7 17 20 2 4 13 9)))

(define (test-in-order-walk)
  (in-order-walk t1 (lambda (x) (- 0 x))))

(define (test-search)
  (display (tree-search '() 3)) (newline)
  (display (tree-search t1 5)) (newline)
  (display (tree-search t1 18)))

(define (test-min-max)
  (display (tree-min '())) (newline)
  (display (tree-min t1)) (newline)
  (display (tree-max '())) (newline)
  (display (tree-max t1)))

(define (test-succ-pred)
  (display (key (succ t1 (tree-search t1 7)))) (newline)
  (display (key (succ t1 (tree-search t1 13)))) (newline)
  (display (key (pred t1 (tree-search t1 6)))) (newline)
  (display (key (pred t1 (tree-search t1 7)))))

(define (test-del)
  (display (tree-delete t1 17)) (newline)
  (display (tree-delete t1 7)) (newline)
  (display (tree-delete t1 6)) (newline)
  (display (tree-delete t1 15)) (newline)
  (display (tree-delete t1 5)))

;; reference
;; literal programming http://en.literateprograms.org/Binary_search_tree_(Scheme)
;; sicp http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-16.html#%_sec_2.3.3
;; end
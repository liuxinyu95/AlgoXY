;; rbtree.scm
;; Copyright (C) 2010 Liu Xinyu (liuxinyu95@gmail.com)
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Definition
(define (make-rbtree c l k r) ;;color, left, key, right
  (list l k c r))

;; Helper functions
(define (key tree) (cadr tree))
(define (color tree) (caddr tree))
(define (left tree) 
  (if (null? tree) '() (car tree)))
(define (right tree)
  (if (null? tree) '() (cadddr tree)))

;; Insertion
(define (red? t)
  (if (null? t) '() (equal? (color t) "R")))

(define (black? t)
  (if (null? t) '() (equal? (color t) "B")))

;; core function
(define (balance c l k r)
  (if (equal? c "B")
      (cond ((and (red? l) (red? (left l)))
	     (make-rbtree "R" 
			  (make-rbtree "B" (left (left l)) (key (left l)) (right (left l)))
			  (key l)
			  (make-rbtree "B" (right l) k r)))
	    ((and (red? l) (red? (right l)))
	     (make-rbtree "R"
			  (make-rbtree "B" (left l) (key l) (left (right l)))
			  (key (right l))
			  (make-rbtree "B" (right (right l)) k r)))
	    ((and (red? r) (red? (right r)))
	     (make-rbtree "R" 
			  (make-rbtree "B" l k (left r))
			  (key r)
			  (make-rbtree "B" (left (right r)) (key (right r)) (right (right r)))))
	    ((and (red? r) (red? (left r)))
	     (make-rbtree "R"
			  (make-rbtree "B" l k (left (left r)))
			  (key (left r))
			  (make-rbtree "B" (right (left r)) (key r) (right r))))
	    (else (make-rbtree c l k r)))
      (make-rbtree c l k r)))

(define (rb-insert tree x)
  (define (make-black t)
    (make-rbtree "B" (left t) (key t) (right t)))
  (define (ins t x)
    (cond ((null? t) (make-rbtree "R" '() x '()))
	  ((< x (key t)) (balance (color t) (ins (left t) x) (key t) (right t)))
	  (else (balance (color t) (left t) (key t) (ins (right t) x)))))
  (make-black (ins tree x)))


;; deletion
(define (dblack? t)
  (if (null? t) '() (equal? (color t) "BB")))

(define (set-color c t)
  (make-rbtree c (left t) (key t) (right t)))

(define (make-black parent t)
  (if (null? t)
      (if (leaf? parent) (set-color "BB" parent) parent)
      (if (red? t) (set-color "B" t) (set-color "BB" t))))

(define (leaf? x)
  (if (null? x) '() (and (null? (left x)) (null? (right x)))))

(define (tree-min tree) 
  (if (null? (left tree)) 
      tree 
      (tree-min (left tree)))) 

(define (fix-dblack c l k r)
  (cond 
   ;;case 1, the sibling is black, and it has one red child
   ((and (dblack? l) (black? r) (red? (left r)))
    (make-rbtree c 
		 (make-rbtree "B" (set-color "B" l) k (left (left r)))
		 (key (left r))
		 (make-rbtree "B" (right (left r)) (key r) (right r))))
   ((and (dblack? l) (black? r) (red? (right r)))
    (make-rbtree c
		 (make-rbtree "B" (set-color "B" l) k (left r))
		 (key r)
		 (set-color "B" (right r))))
   ((and (dblack? r) (black? l) (red? (right l)))
    (make-rbtree c
		 (make-rbtree "B" (left l) (key l) (left (right l)))
		 (key (right l))
		 (make-rbtree "B" (right (right l)) k (set-color "B" r))))
   ((and (dblack? r) (black? l) (red? (left l)))
    (make-rbtree c
		 (set-color "B" (left l))
		 (key l)
		 (make-rbtree "B" (right l) k (set-color "B" r))))
   ;;case 2, the sibling and its 2 children are all black,
   ;;        propagate the blackness up
   ((and (dblack? l) (black? r) (black? (left r)) (black? (right r)))
    (make-black '() (make-rbtree c
				 (set-color "B" l)
				 k
				 (set-color "R" r))))
   ((and (dblack? r) (black? l) (black? (left l)) (black? (right l)))
    (make-black '() (make-rbtree c
				 (set-color "R" l)
				 k
				 (set-color "B" r))))
   ;;case 3, the silbing is red
   ((and (dblack? l) (red? r))
    (fix-dblack "B" (fix-dblack "R" l k (left r)) (key r) (right r)))
   ((and (dblack? r) (red? l))
    (fix-dblack "B" (left l) (key l) (fix-dblack "R" (right l) k r)))
   (else (make-rbtree c l k r))))

(define (rb-delete tree x) ;; x is a value, not a node
  (define (blacken-root t)
    (if (null? t) '() (set-color "B" t)))
  (define (del t x)
    (cond ((null? t) '())
	  ((< x (key t)) (fix-dblack (color t) 
				     (del (left t) x)
				     (key t)
				     (right t)))
	  ((> x (key t)) (fix-dblack (color t)
				     (left t)
				     (key t)
				     (del (right t) x)))
	  ((null? (left t)) (if (black? t)
				(make-black t (right t))
				(right t)))
	  ((null? (right t)) (if (black? t)
				 (make-black t (left t))
				 (left t)))
	  (else (let ((newkey (key (tree-min (right t)))))
		  (fix-dblack (color t)
			      (left t)
			      newkey
			      (del (right t) newkey))))))
  (blacken-root (del tree x)))
;; test and helpers

(define (list->rbtree lst)
  (fold-left rb-insert '() lst))

(define t1 (list->rbtree '(11 2 14 1 7 15 5 8 4)))
(define t2 (list->rbtree '(1 2 3 4 5 6 7 8)))

(define (test-del) 
  (display (rb-delete t1 4)) (newline) 
  (display (rb-delete t1 5)) (newline) 
  (display (rb-delete t1 2)) (newline) 
  (display (rb-delete t1 7)) (newline) 
  (display (rb-delete t1 14)) (newline) 
  (display (rb-delete t1 3)))

#|
    splayheap.scm, splay heap in Scheme.
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

;; include some generic heap functions
(load "genheap.scm")

;; for pretty print purpose, the splay tree is arranged as
;; (left element right)

;; access functions

(define (left t)
  (if (null? t) '() (car t)))

(define (elem t)
  (if (null? t) '() (cadr t)))

(define (right t)
  (if (null? t) '() (caddr t)))

;; constructor
(define (make-tree l x r) ;; l: left, x: element, r: right
  (list l x r))

;; partition the tree in 2 parts based on pivot value
(define (partition t pivot)
  (if (null? t) 
      (cons '() '())
      (let ((l (left t))
	    (x (elem t))
	    (r (right t)))
	(if (< x pivot)
	    (if (null? r)
		(cons t '())
		(let ((l1 (left r))
		      (x1 (elem r))
		      (r1 (right r)))
		  (if (< x1 pivot)
		      (let* ((p (partition r1 pivot))
			     (small (car p))
			     (big (cdr p)))
			(cons (make-tree (make-tree l x l1) x1 small) big))
		      (let* ((p (partition l1 pivot))
			     (small (car p))
			     (big (cdr p)))
			(cons (make-tree l x small) (make-tree big x1 r1))))))
	    (if (null? l)
		(cons '() t)
		(let ((l1 (left l))
		      (x1 (elem l))
		      (r1 (right l)))
		  (if (> x1 pivot)
		      (let* ((p (partition l1 pivot))
			     (small (car p))
			     (big (cdr p)))
			(cons small (make-tree l1 x1 (make-tree r1 x r))))
		      (let* ((p (partition r1 pivot))
			     (small (car p))
			     (big (cdr p)))
			(cons (make-tree l1 x1 small) (make-tree big x r))))))))))
      
(define (insert t x)
  (let* ((p (partition t x))
	 (small (car p))
	 (big (cdr p)))
    (make-tree small x big)))

(define (merge t1 t2)
  (if (null? t1)
      t2
      (let* ((p (partition t2 (elem t1)))
	     (small (car p))
	     (big (cdr p)))
	(make-tree (merge (left t1) small) (elem t1) (merge (right t1) big)))))

;; over-write definition in genheap.scm
(define (find-min t)
  (if (null? (left t)) 
      (elem t)
      (find-min (left t))))

;; over-write
(define (delete-min t)
  (cond ((null? (left t)) (right t))
	((null? (left (left t))) (make-tree (right (left t)) (elem t) (right t)))
	(else (make-tree (delete-min (left (left t)))
			 (elem (left t))
			 (make-tree (right (left t)) (elem t) (right t))))))

(define (splay l x r y)
  (cond ((eq? y (elem (left l))) ;; zig-zig
	 (make-tree (left (left l)) 
		    (elem (left l))
		    (make-tree (right (left l)) 
			       (elem l) 
			       (make-tree (right l) x r))))
	((eq? y (elem (right r))) ;; zig-zig
	 (make-tree (make-tree (make-tree l x (left r))
			       (elem r)
			       (left (right r)))
		    (elem (right r))
		    (right (right r))))
	((eq? y (elem (right l))) ;; zig-zag
	 (make-tree (make-tree (left l) (elem l) (left (right l)))
		    (elem (right l))
		    (make-tree (right (right l)) x r)))
	((eq? y (elem (left r))) ;; zig-zag
	 (make-tree (make-tree l x (left (left r)))
		    (elem (left r))
		    (make-tree (right (left r)) (elem r) (right r))))
	((eq? y (elem l)) ;; zig
	 (make-tree (left l) (elem l) (make-tree (right l) x r)))
	((eq? y (elem r)) ;; zig
	 (make-tree (make-tree l x (left r)) (elem r) (right r)))
	(else (make-tree l x r))))

(define (insert-splay t x)
  (cond ((null? t) (make-tree '() x '()))
	((> (elem t) x)
	 (splay (insert-splay (left t) x) (elem t) (right t) x))
	(else
	 (splay (left t) (elem t) (insert-splay (right t) x) x))))

(define (lookup t x)
  (cond ((null? t) '())
	((= x (elem t)) t)
	((> x (elem t))
	 (splay (lookup (left t) x) (elem t) (right t) x))
	(else
	 (splay (left t) (elem t) (lookup (right t) x) x))))

;; from-list, heap-sort is reused in genheap.scm

(define (from-list-splay lst)
  (fold-left insert-splay '() lst))

(define (test-from-list-splay)
  (from-list-splay '(16 14 10 8 7 9 3 2 4 1)))
	
;; random test of partition
(define (test-partition)
  (define (rand-list n)
    (if (= n 0)
	'()
	(cons (random 10) (rand-list (- n 1)))))
  (let ((a (from-list (rand-list 100)))
	(b (from-list (rand-list 100))))
    (merge a b)))
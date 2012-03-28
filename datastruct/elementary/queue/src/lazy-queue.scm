#|
 lazy-queue.scm, A real time O(1) queue by lazy evaluation
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
;;   A queue is consist of a front list, a rear list, and a stream
;;   The stream is delayed incremental computation for 
;;       (append f (reverse r))
(define (make-queue f r s)
  (list f r s))

;; Auxiliary functions
(define (front-lst q) (car q))

(define (rear-lst q) (cadr q))

(define (rots q) (caddr q))

;; Queue interfaces
(define empty (make-queue '() '() '()))

(define (empty? q) (stream-null? (front-lst q)))

(define (push q x)
  (balance (front-lst q) (cons x (rear q)) (rots q)))

;; Skip the error handling
(define (pop q)
  (balance (stream-cdr (front-lst q)) (rear q) (rots q)))

(define (front q) (stream-car (front-lst q)))

(define (balance f r s)
  (if (stream-null? s)
      (let ((newf (rotate f r '())))
	(make-queue newf '() newf))
      (make-queue f r (stream-cdr s))))

;; invariant: |xs| + 1 == |ys| 
(define (rotate xs ys acc)
  (if (stream-null? xs)
      (cons-stream (car ys) acc)
      (cons-stream (stream-car xs) 
		   (rotate (stream-cdr xs) (cdr ys) 
			   (cons-stream (car ys) acc)))))

;; Helper function
(define (list->queue lst)
  (fold-left push empty lst))

;; testing
(define (rand-seq n m)
  (map (lambda (x) (random m)) (iota n)))

(define (test-proc lst q enqueue dequeue front-elem is-empty?)
  (define (proc lst q)
    (cond ((null? lst) '())
	  ((even? (car lst)) (proc (cdr lst) (enqueue q (car lst))))
	  ((is-empty? q) (proc (cdr lst) q))
	  (else (cons (front-elem q) (proc (cdr lst) (dequeue q))))))
  (proc lst q))

(define (proc1 xs)
  (test-proc xs '() 
	     (lambda (lst x) (append lst (list x)))
	     cdr car null?))

(define (proc2 xs)
  (test-proc xs empty push pop front empty?))

;; test n random test cases
(define (test n)
  (define (assert exp)
    (if (not exp) (error "fail" exp)))
  (define (test-queue i)
    (let* ((m (random 1000))
	   (xs (rand-seq m 1000)))
      (assert (equal? (proc1 xs) (proc2 xs)))))
  (for-each test-queue (iota n)))
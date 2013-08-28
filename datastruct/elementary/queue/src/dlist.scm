;; dlist.scm
;; Copyright (C) 2013 Liu Xinyu (liuxinyu95@gmail.com)
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

;; doubly linked-list by zipper approach
;; doubly linked-list =  reversed(front) + rear

;; definition
(define (make-lst f r) (list f r))

;; empty value
(define empty-lst (make-lst '() '()))

;; accessors
(define (front lst) (car lst))

(define (rear lst) (cadr lst))

;; navigation
;; O(1) if rear isn't empty, O(n) otherwise;
;; amortized O(1)
(define (forward lst)
  (if (null? (rear lst))
      (let ((r (reverse (front lst))))
        (make-lst (list (car r))
                   (cdr r)))
      (make-lst (cons (car (rear lst)) (front lst))
                 (cdr (rear lst)))))

;; O(1) if front isn't empty, O(n) otherwise;
;; amortized O(1)
(define (backward lst)
  (if (null? (front lst))
      (let ((f (reverse (rear lst))))
        (make-lst (cdr f) (list (car f))))
      (make-lst (cdr (front lst))
                 (cons (car (front lst)) (rear lst)))))
                 
;; insertion before the current focus, O(1)
(define (insert-lst x lst)
  (make-lst (front lst) (cons x (rear lst))))

;; appending after the current focus, O(1)
(define (append-lst lst x)
  (make-lst (cons x (front lst)) (rear lst)))

;; convert to a plain list, linear O(n) time
(define (dlist->list lst)
  (append (rear lst) (reverse (front lst))))

;; convert from a plain list, linear O(n) time
(define (from-list lst)
  (fold-right (lambda (x dlst) (insert-lst x dlst)) empty-lst lst))

;; todo: implement delete


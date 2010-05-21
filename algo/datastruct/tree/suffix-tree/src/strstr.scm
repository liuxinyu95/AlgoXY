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

(define (compare-on func)
  (lambda (x y) 
    (cond ((< (func x) (func y)) 'lt)
	  ((> (func x) (func y)) 'gt)
	  (else 'eq))))

(define (max-by comp lst)
  (define (update-max xs x)
    (case (comp (car xs) x)
      (('lt) (list x))
      (('gt) lst)
      (else (cons x lst))))
  (if (null? lst)
      '()
      (fold-left update-max (list (car lst)) (cdr lst))))

(define (lrs t)
  (define (find lst)
    (if (null? lst)
	'("")
	(let* ((s (edge (car lst)))
	       (tr (children (car lst))))
	  (max-by (compare-on string-length) 
		  (append
		   (map (lambda (x) (string-append s x)) (lsr tr))
		   (find (cdr lst)))))))
  (if (leaf? t)
      '("")
      (find (filter (lambda (x) (not (leaf? x))) (children t)))))
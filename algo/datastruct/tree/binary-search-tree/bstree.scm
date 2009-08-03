;; Definition
(define (make-tree left key right)
  (list left key right))

;; Helper functions
(define (key tree) (cadr tree))
(define (left tree) (car tree))
(define (right tree) (caddr tree))

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

;; reference
;; literal programming http://en.literateprograms.org/Binary_search_tree_(Scheme)
;; sicp http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-16.html#%_sec_2.3.3
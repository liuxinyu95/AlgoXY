(define (make-tree left key right)
  (list left key right))

(define (key tree) (cadr tree))
(define (left tree) (car tree))
(define (right tree) (caddr tree))

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

;; reference
;; literal programming http://en.literateprograms.org/Binary_search_tree_(Scheme)
;; sicp http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-16.html#%_sec_2.3.3
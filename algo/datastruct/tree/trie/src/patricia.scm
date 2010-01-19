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

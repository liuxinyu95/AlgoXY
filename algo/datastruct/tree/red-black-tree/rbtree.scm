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

;; test and helpers

(define (list->rbtree lst)
  (fold-left rb-insert '() lst))

(define t1 (list->rbtree '(11 2 14 1 7 15 5 8 4)))
(define t2 (list->rbtree '(1 2 3 4 5 6 7 8)))
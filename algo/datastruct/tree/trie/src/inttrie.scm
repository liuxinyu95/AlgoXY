;; Integer Trie

;; Definition

(define (make-int-trie l v r) ;; left, value, right
  (list l v r))

;; Helpers
(define (left trie)
  (if (null? trie) '() (car trie)))

(define (value trie)
  (if (null? trie) '() (cadr trie)))

(define (right trie)
  (if (null? trie) '() (caddr trie)))

;; Insertion
;;   if user insert an existed value, just overwrite the old value
;;   usage: (insert t k x) t: trie, k: key, x: value
(define (insert t k x)
  (if (= k 0)
      (make-int-trie (left t) x (right t))
      (if (even? k)
	  (make-int-trie (insert (left t) (/ k 2) x) (value t) (right t))
	  (make-int-trie (left t) (value t) (insert (right t) (/ (- k 1) 2) x)))))

;; Lookup
(define (lookup t k)
  (if (null? t) '()
      (if (= k 0) (value t)
	  (if (even? k) 
	      (lookup (left t) (/ k 2))
	      (lookup (right t) (/ (- k 1) 2))))))

(define (list->trie lst) ;;lst is list of pairs
  (define (insert-pair t p)
    (insert t (car p) (cadr p)))
  (fold-left insert-pair '() lst))

;; Test helpers

(define (trie->string trie)
  (define (value->string x)
    (cond ((null? x) ".")
	  ((number? x) (number->string x))
	  ((string? x) x)
	  (else "unknon value")))
  (define (trie->str t k m)
    (if (null? t)
	"."
	(string-append "(" (trie->str (left t) k (* m 2)) " "
		       (number->string k) (value->string (value t)) " "
		       (trie->str (right t) (+ m k) (* m 2)) ")")))
  (trie->str trie 0 1))
	
(define (test-int-trie)
  (define t (list->trie (list '(1 "a") '(4 "b") '(5 "c") '(9 "d"))))
  (display (trie->string t)) (newline)
  (display "lookup 4: ") (display (lookup t 4)) (newline)
  (display "lookup 0: ") (display (lookup t 0)) (newline))

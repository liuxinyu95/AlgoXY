;; appending

(define (append lst x)
  (if (null? lst) 
      (list x) 
      (cons (car lst) (append (cdr lst) x))))
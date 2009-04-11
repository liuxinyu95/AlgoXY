(define (fact1 n)
  (if (= n 1) 1 (* n (fact1 (- n 1)))))

;(fact1 6) = 720

(define fact2
  (lambda(f n)
    (if (= n 1)
    	1
	(* n (f f (- n 1))))))

;(fact2 fact2 6) = 720

; func(n) = ((fact3)(f))(n)
(define (fact3 f)
  (lambda (n) (if (= n 1) 1 (* n ((f f) (- n 1)))))) ;(f f) is recursive func (*)

;((fact3 fact3) 6) = 720

(define (fact-only f1)
  (lambda (n) (if (= n 1) 1 (* n (f1 (- n 1)))))) ; f1 is recursive func, it IS (f f) in (*)

; func(n) = ((fact4)(f))(n), where f is fact4 itself to recursive
(define (fact4 f2)
  (lambda (n) ((fact-only (f2 f2)) n))) ; (f2 f2) --> f1 --> (f f)
                          
;((fact4 fact4) 6) = 720

;fact-only --> parameter f
; change name f2 --> x
(define (fact5 f)
  (local ((define (fact-4 x)
           (lambda (n) ((f (x x)) n))))
    (fact-4 fact-4)))

; remove the local fact-4 by using lambda
(define (fact6 f)
  (
   (lambda(x) (lambda (n) ((f (x x)) n)))
   (lambda(x) (lambda (n) ((f (x x)) n)))
  ))

;( (fact6 fact-only) 6) = 720.
;fact6 --> Y

(define Y
  (lambda (f)
    (
     (lambda(x) (lambda (n) ((f (x x)) n)))
     (lambda(x) (lambda (n) ((f (x x)) n))))))

(define (fact n)
((lambda (n)
  ((lambda (fact)
      (fact fact n))
   (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1)))))))
n))

    
(define result
  ((lambda (n) ((lambda (f) (f f n)) (lambda (f n ) (if (= n 0 ) 1 (* n (f f (- n 1))))))) 6))


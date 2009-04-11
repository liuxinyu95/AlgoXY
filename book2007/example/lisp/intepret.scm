;;; main proc: eval -- apply

(define (eval exp env)
  (cond ((self-eval? exp) exp)
	((var? exp) (lookup-var-val exp env))
	;((quoted? exp) (text-of exp))
	;((assign? exp) (eval-assign exp env))
	((def? exp) (eval-def exp env))
	((if? exp) (eval-if exp env))
	((lambda? exp)
	 (make-proc (lambda-params exp)
		    (lambda-body exp)
		    env))
	;((begin? exp)
	; (eval-seq (begin-actions exp) env))
	;((cond? exp) (eval (cond->if exp) env))
	((app? exp)
	 (meta-apply (eval (operator exp) env)
		(list-of-val (operands exp) env)))
	(else
	 (error "Unknown expression type -- EVAL" exp))))

(define (meta-apply proc args)
  (cond ((prim-proc? proc)
	 (apply-prim-proc proc args))

	((compound-proc? proc)
	 (eval-seq
	  (proc-body proc)
	  (extend-env
	   (proc-params proc) args (proc-env proc))))

	(else (error "Unknown proc type --APPLY" proc))))

;;; helper procs

;;; list-of-val
;;;   used in eval app/eval begin ...
(define (list-of-val exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
	    (list-of-val (rest-operands exps) env))))

;;; if condition

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

;;; eval-seq
(define (eval-seq exps env)
  (if (last-exp? exps) 
      (eval (first-exp exps) env)
      (eval-seq (rest-exps exps) env)))

;;; eval-def
(define (eval-def exp env)
  (def-var! (def-var exp) (eval (def-val exp) env) env)
  'ok)

;;; basic eval type test proc

(define (self-eval? exp)
  (cond ((number? exp) true)
	((string? exp) true)
	(else false)))

(define (var? exp) (symbol? exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;;; used in eval
;;;  the form is (define <var> <value>) or
;;;              (define (<var> <param1> ... <param_n>) <body>))

(define (def? exp) (tagged-list? exp 'define ))

;;; used in eval-def
(define (def-var exp)
  (if (symbol? (car (cdr exp)))
      (car (cdr exp)) ; form (define <var> <value>)
      (car (car (cdr exp))))) ; form (define (<var> <param_1>...<param_n>) <body>))

(define (def-val exp)
  (if (symbol? (cadr exp))
      (caddr exp) ; form (define <var> <value>)
      (make-lambda (cdadr exp) ; formal params <param_1>...<param_n>
		   (cddr exp)))); <body>

;;; lambda stuffs

(define (lambda? exp) (tagged-list? exp 'lambda ))

(define (lambda-params exp) (cadr exp)) ; (lambda (<params>) (<body>))

(define (lambda-body exp) (cddr exp))

(define (make-lambda params body)
  (cons 'lambda (cons params body)))

;;; if condition stuffs

(define (if? exp) (tagged-list? exp 'if)) ; ('if <pred> <conseq> <alter>)
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

;;; sequence stuffs

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

;;; application stuffs

(define (app? exp) (pair? exp)); (app <params>)

(define (operator exp) (car exp)); 'app

(define (operands exp) (cdr exp)); <param1>...<param_n>

;;; used in list-of-val
(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops)); (app p1 p2 ...pN), 1st oprand is p1

(define (rest-operands ops) (cdr ops)); p2 ... pN

;;; TODO: test true/false

;;; primitive proc stuffs

(define (prim-proc? proc)
  (tagged-list? proc 'prim)) ; for example: ('prim +)

(define (prim-impl proc) (cadr proc)) ; +

(define prim-procs
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
	(list '- -)
	(list '* *)
	(list '/ /)
	(list '= =)
	(list '< <) ; more...
        ))

(define (prim-proc-names)
  (map car
       prim-procs)) ;('car 'cdr 'cons 'null? '+ '- ... )

(define (prim-proc-objects)
  (map (lambda (proc) (list 'prim (cadr proc)))
       prim-procs)) ; ('lambda(p) ('prim (cadr p)) --> ('+ +))
                    ; ('prim +)

;;; apply prim proc
;;; used in apply

(define (apply-prim-proc proc args)
  (apply (prim-impl proc) args))
;  ((prim-impl proc) args)) ;can this work?

;;; compound proc stuffs

(define (make-proc params body env)
  (list 'procedure params body env ))

;;; used in apply
; for example: ('procedure (my-proc 2 3))
(define (compound-proc? p) (tagged-list? p 'procedure ))

(define (proc-params p) (cadr p))

(define (proc-body p) (caddr p))

(define (proc-env p) (cadddr p))

;;; env stuffs

(define (outter-env env) (cdr env)) ; outter-env is also called as base-env

(define (first-frame env) (car env))

(define empty-env '() )

;;; frame stuffs

(define (make-frame vars vals) (cons vars vals))

(define (frame-vars frame) (car frame))

(define (frame-vals frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-env vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (error "Number of vars and vals mismatch." vars vals)))

(define (lookup-var-val var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (outter-env env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env empty-env)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-vars frame)
                (frame-vals frame)))))
  (env-loop env))

;;; TODO: set-var-val!

;;; def-var!
(define (def-var! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-vars frame)
          (frame-vals frame))))

;;; setup the whole program

(define (setup-env)
  (let ((init-env
         (extend-env (prim-proc-names)
                     (prim-proc-objects)
                     empty-env)))
    (def-var! 'true  true  init-env)
    (def-var! 'false false init-env)
    init-env))

(define global-env (setup-env))

;;; driver loop

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input global-env)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-proc? object)
      (display (list 'compound-proc
                     (proc-params object)
                     (proc-body object)
                     ;'<procedure-env>
		     ))
      (display object)))

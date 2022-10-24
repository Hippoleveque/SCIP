#| Exercise 4.1 |#

 #| left to right |#
(define (list-of-values exps env)
    (if (no-operands? exps)
        '()
        (let ((left (eval (first-operand exps) env)))
            (cons left
                (list-of-values (rest-operands exps) env)
            )
        )
    )
)

#| right to left |#

(define (list-of-values exps env)
    (if (no-operands? exps)
        '()
        (let ((right (list-of-values (rest-operands exps) env)))
            (cons (eval (first-operand exps) env)
                right
            )
        )
    )
)

#| Exercise 4.2 |#

#| a. |#

#| the evaluator 
will try to apply 
the procedure define 
to x and 3, try 
to find the body 
of the procedure and it will be unable to
|#

#| b. |#

(define (application? expr) (tagged-list? expr 'call))

(define (operator expr) (cadr expr))

(define (operands expr) (cddr expr))

#| Exercise 4.3 |#

#| original eval |#

(define (eval expr env)
    (cond ((self-evaluating? expr) expr)
        ((variable? expr) (lookup-variable-value expr env))
        ((quoted? expr) (text-of-quotation expr))
        ((assignment? expr) (eval-assigment expr env))
        ((definition? expr) (eval-definition expr env))
        ((if? expr) (eval-if expr env))
        ((lambda? expr) (make-procedure 
                (lambda-parameters expr)
                (lambda-body expr)
                env
        ))
        ((begin? expr)
        (eval-sequence (begin-actions expr) env)
        )
        ((cond? expr) (eval (cond->if expr) env))
        ((application? expr) (my-apply (eval (operator expr) env)
                                        (list-of-values (operands expr) env)
                                )
        )
        (else
            (error "Unknown expression type -- EVAL" expr)
        )
    )
)

#| data-directed eval |#

(define (eval expr env)
    (cond ((self-evaluating? expr) expr)
          ((variable? expr) (lookup-variable-value expr env))
          ((get 'eval (car expr)) ((get 'eval (car expr)) (cdr expr) env))
          ((application? expr) (my-apply (eval (operator expr) env)
              (list-of-values (operands expr) env)
          )
          )
          (else
              (error "Unknown expression type -- EVAL" expr)
          )
    )
)

(define (install-eval-package)
    (define (eval-lambda expr env)
        (make-procedure (lambda-parameters expr) (lambda body) env)
    )

    (define (eval-begin expr env) 
        (eval-sequence (begin-actions expr) env)
    )

    (define (eval-cond expr env)
        (eval (cond-if expr) env)
    )

    (define (eval-application expr env)
        (my-apply (eval (operator expr) env)
                  (list-of-values (operands expr) env)
        )
    )

    (put 'eval 'quote text-of-quotation)
    (put 'eval 'set! deriv-prod)

    ; Not beautiful but ok
)


#| Exercise 4.4 |#


(define (eval expr env)
    (cond ((self-evaluating? expr) expr)
        ((variable? expr) (lookup-variable-value expr env))
        ((quoted? expr) (text-of-quotation expr))
        ((assignment? expr) (eval-assigment expr env))
        ((definition? expr) (eval-definition expr env))
        ((if? expr) (eval-if expr env))
        ((lambda? expr) (make-procedure 
                (lambda-parameters expr)
                (lambda-body expr)
                env
        ))
        ((begin? expr)
        (eval-sequence (begin-actions expr) env)
        )
        ((and? expr) (eval-and expr env))
        ((or? expr) (eval-or expr env))
        ((cond? expr) (eval (cond->if expr) env))
        ((application? expr) (my-apply (eval (operator expr) env)
                                        (list-of-values (operands expr) env)
                                )
        )
        (else
            (error "Unknown expression type -- EVAL" expr)
        )
    )
)

#| Direct implementations |#

(define (and? expr) (tagged-list? expr 'and))

(define (or? expr) (tagged-list? expr 'or))

(define (clauses expr)
    (cdr expr)
)

(define (first-clause clauses)
    (car clauses)
)

(define (rest-clauses clauses)
    (cdr clauses)
)

(define (eval-and expr env)
    (eval-and-clauses (and-clauses expr) env)
)

(define (eval-and-clauses clauses env)
    (cond ((null? clauses) #t)
          ((eval (first-clause clauses) env) (eval-and-clauses (rest-clauses clauses) env))
          (else #f)

    )
)

(define (eval-or expr env)
    (eval-or-clauses (or-clauses expr) env)
)

(define (eval-or-clauses clauses env)
    (cond ((null? clauses) #f)
          ((eval (first-clause clauses) env) #t)
          (else (eval-or-clauses (rest-clauses clauses) env))
    )
)

#| Implementations as derived expressions |#

(define (and->if expr)
    (expand-and-clauses (clauses expr))
)

(define (expand-and-clauses clauses)
    (if (null? clauses) #t
        (let ((first (first-clause clauses))
              (rest (rest-clauses clauses))
             )
             (make-if first (expand-and-clauses rest)
                             #f
             )
        )
    )
)

(define (or->if expr)
    (expand-or-clauses (clauses expr))
)

(define (expand-or-clauses clauses)
    (if (null? clauses) #t
        (let ((first (first-clause clauses))
            (rest (rest-clauses clauses))
            )
            (make-if first #t
                           (expand-or-clauses rest)
            )
        )
    )
)

#| Exercise 4.5 |#

(define (is-arrow-clause? clause)
    (eq? '=> (cadr clause))
)

(define (cond-recipient clause)
    (caddr clause)
)

(define (expand-clauses clauses)
    (if (null? clauses)
        #f
        (let ((first (car clauses))
            (rest (cdr clauses))
            )
            (if (cond-else-clause? first)
                (if (null? rest)
                    (sequence->exp (cond-actions first))
                    (error "ELSE clause isn't last -- COND->IF", clauses)
                )
                (if (is-arrow-clause? first)
                    (make-if (cond-predicate first)
                             ((cond-recipient) (cond-predicate first))
                             (expand-clauses rest)
                    )
                    (make-if (cond-predicate first)
                        (cond-actions first)
                        (expand-clauses rest)
                    )
                )   
            )    
        )
    )
)

#| Bad solution as the the cond-predicate first is evaluated twice
I did not find any good solution online 
|#

#| Exercise 4.6 |#

(define (eval expr env)
    (cond ((self-evaluating? expr) expr)
        ((variable? expr) (lookup-variable-value expr env))
        ((quoted? expr) (text-of-quotation expr))
        ((assignment? expr) (eval-assigment expr env))
        ((definition? expr) (eval-definition expr env))
        ((if? expr) (eval-if expr env))
        ((lambda? expr) (make-procedure 
                (lambda-parameters expr)
                (lambda-body expr)
                (env)
        ))
        ((begin? expr)
        (eval-sequence (begin-actions expr) env)
        )
        ((let? expr) (eval (let->combination) env))
        ((cond? expr) (eval (cond->if expr) env))
        ((application? expr) (my-apply (eval (operator expr) env)
                                        (list-of-values (operands expr) env)
                                )
        )
        (else
            (error "Unknown expression type -- EVAL" expr)
        )
    )
)

(define (let? expr)
    (tagged-list? expr 'let)
)

(define (vars expr)
    (cadr expr)
)

(define (body expr)
    (cddr expr)
)

(define (parameters expr)
    (map car (vars expr))
)

(define (exprs expr)
    (map cdr (vars expr))
)

(define (let->combination expr env)
    (cons (make-lambda (parameters expr) (body expr))
          (exprs expr)
    )
)

#| Exercise 4.7 |#

(define (let* expr)
    (tagged-list? expr 'let*)
)

(define (let-expr expr)
    (cdr expr)
)

(define (vars expr)
    (cadr expr)
)

(define (variables expr)
    (map car (cadr expr))
)

(define (exprs expr)
    (map cdr (cadr expr))
)

(define (make-let args body)
    (cons 'let 
        (cons
            args
            body
        )  
    )
)

(define (let*->nested-lets expr)
    (expand-let* (vars expr) body)
)

(define (expand-let* args body)
    (if (null? args)
        body
        (make-let (list (car args))
                  (list (expand-let* (cdr args) body))
        )
    )
)

#| Exercise 4.8 |#

(define (bindings expr)
    (if (pair? (caadr expr))
        (cadr expr)
        (caddr expr)
    )
)

(define (first-var expr)
    (if (pair? (caadr expr))
        #f
        (caddr expr)
    )
)

(define (body expr)
    (if (pair? (caadr expr))
        (cddr expr)
        (cdddr expr)
    )
)

(define (parameters bindings)
    (map car bindings)
)

(define (exprs bindings)
    (map cdr bindings)
)


#| (define (let->combination expr env)
    (if (pair? (car expr))
        (cons (make-lambda (parameters (bindings expr)) (body expr))
            (exprs (bindings expr))
        )
        (let ((proc (make-lambda (cons (first-var expr) (parameters (bindings expr))) 
                                 (body expr)
                    ) 
                ()
                (list (list 'define (first-var expr) proc) 
                      
                )
        )
    )
) |#

#| Won't do  |#


#| Exercise 4.9 |#

(define (eval expr env)
    (cond ((self-evaluating? expr) expr)
        ((variable? expr) (lookup-variable-value expr env))
        ((quoted? expr) (text-of-quotation expr))
        ((assignment? expr) (eval-assigment expr env))
        ((definition? expr) (eval-definition expr env))
        ((if? expr) (eval-if expr env))
        ((lambda? expr) (make-procedure 
                (lambda-parameters expr)
                (lambda-body expr)
                (env)
        ))
        ((while? expr) (eval (while->combination expr) env))
        ((begin? expr)
        (eval-sequence (begin-actions expr) env)
        )
        ((cond? expr) (eval (cond->if expr) env))
        ((application? expr) (my-apply (eval (operator expr) env)
                                        (list-of-values (operands expr) env)
                                )
        )
        (else
            (error "Unknown expression type -- EVAL" expr)
        )
    )
)

(define (while? expr) (tagged-list? expr 'while)) 
(define (while-condition expr) (cadr expr)) 
(define (while-body expr) (cddr expr)) ; Be careful, we can have a multi-expression body
(define (while->combination expr) 
        (sequence->exp 
                (list (list 'define  
                            (list 'while-iter) 
                            (make-if (while-condition expr)  
                                (sequence->exp (list (while-body expr)  
                                (list 'while-iter))) 
                                 'true
                            )
                        )    
                    (list 'while-iter)
                )
        )
) 

#| Solution found online |#


#| Exercise 4.10 |#

#| Will do in another repo or at least in another folder |#

#| Exercise 4.11 |#

(define (make-frame variables values)
    (cons 'frame
          (map cons variables values)
    )
)   

(define (bindings frame)
    (cdr frame)
)

(define (add-binding-to-frame! var val frame)
    (set-cdr! frame (cons (cons var val) (bindings frame)))
)

(define (lookup-variable-value var env)
    (define (env-loop env)
        (define (scan bindings)
            (cond ((null? bindings) (env-loop (enclosing-environment env)))
                ((eq? var (caar bindings)) (cdar bindings))
                (else (scan (cdr bindings))
            )
        )
        (if (eq? env the-empty-environment)
            (error "Unbound variable" var)
            (let ((frame (first-frame env)))
                (scan (bindings frame))
            )
        )
    )
    (env-loop env)
))

(define (set-variable-value! var val env)
    (define (env-loop env)
        (define (scan bindings)
            (cond ((null? bindings) (env-loop (enclosing-environment env)))
                ((eq? var (caar bindings)) (set-cdr! (car bindings) val))
                (else (scan (cdr bindings))
            )
        )
        (if (eq? env the-empty-environment)
            (error "Unbound variable" var)
            (let ((frame (first-frame env)))
                (scan (bindings frame))
            )
        )
    )
    (env-loop env)
))

(define (define-variable! var val env)
    (let ((frame (first-frame env)))
        (define (scan bindings)
            (cond ((null? bindings) (add-binding-to-frame! var val frame))
                ((eq? (caar bindings) var)
                (set-cdr! (car bindings) val)
                )
                (else (scan (cdr bindings))
            )
        )
        (scan (bindings frame))
        )
    )
)

#| Exercise 4.12 |#

(define (env-loop env next match)
    (define (scan vars vals)
        (cond ((null? vars) (next env))
              ((eq? var (cars vars)) (match vars vals))
              (else (scan (cdr vars) (cdr vals)))
        )
    )
    (if (eq? env the-empty-environment)
        (error "Empty environment reached.")
        (let ((frame (first-frame env)))
            (scan (frame-variables frame) (frame-values frame))
        )
    )
)

(define (lookup-variable-value var env)
    (env-loop env
              (lambda (the-env) (lookup-variable-value var 
                                                       (enclosing-environment the-env)))
              (lambda (vars vals) (car vals))
    )   
)

(define (set-variable-value! var val env)
    (env-loop env
        (lambda (the-env) (set-variable-value! var val
                                                (enclosing-environment the-env)))
        (lambda (vars vals) (set-car! vals (car vals)))
    )   
)

(define (define-variable! var val env)
    (env-loop env
        (lambda (the-env) (add-binding-to-frame! var val (first-frame env)))    
        (lambda (vars vals) (set-car! vals (car vals)))
    )  
)

#| Exercise 4.13 |#

#| For safety reason, it seems more reasonable to only allow
for unbounding on the first frame. 
Otherwise, a procedure that wants to cleanup its own variable might just 
end up cleaning up variables completely unrelated to it. 
|#

(define (make-unbound! var env)
    (let* ((frame (first-frame env))
            (vars (frame-variables frame))
            (vals (frame-values frame))
          )
        (define (scan vars vals)
            (cond ((null? (cdr vars)) 'done)
                ((eq? var (cadr vars)) (begin (set-cdr! vars (cddr vars)) (set-cdr! vals (cddr vals))))
                (else (scan (cdr vars) (cdr vals)))
            )
        )
         (if (and (eq? (car vars) var) (null? (cdr vars)))
            (set-cdr! frame '())
            (scan vars vals)
         )
    )
)

#| Exercise 4.14 |#

#| 
When Lu Ator types the definition of map, everything goes well, which is normal
When Louis has installed the system version of map as a primitive for 
the circular evaluator.

What happens when Louis tries to evaluate smthing is that 
the evaluator is going to evaluate the proc argument
of the map and either it'll already defined 
in the env as smthg like ('{procedure_name} parameters procedureBody env)
so when map is implemented as a primitive, 
it will try to apply the procedure '({procedure_name} paramters procedureBody env)
to the list and will crash.
|#

#| Exercise 4.15 |#

(define (run-forever) (run-forever))

(define (try p)
    (if (halts? p p)
        (run-forever)
        'halted
    )
)

(try try)

(if (halts? try try)
    (run-forever)
    'halted
)

#| 
When running (try try):
- If (halts? try try), the program runs forever which violates the fact 
it halts on itself 
- If not, it means it never halts but then it displays 'halted
|#


#| Exercise 4.16 |#

#| a. |#

(define (lookup-variable-value var env)
    (define (env-loop env)
        (define (scan vars vals)
            (cond ((null? vars) (env-loop (enclosing-environment env)))
                ((eq? var (car vars)) (if (eq? (car vals) '*unassigned) 
                                          (error "Value is unasigned yet.") 
                                      ))
                (else (scan (cdr vars) (cdr vals)))
            )
        )
        (if (eq? env the-empty-environment)
            (error "Unbound variable" var)
            (let ((frame (first-frame env)))
                (scan (frame-variables frame) (frame-values frame))
            )
        )
    )
    (env-loop env)
)

#| b. |#

(define (scan-out-defines proc-body)
    (let ((internal-def-names '())
          (internal-def-values '())
          (other-body-exprs '())
        )
        (define (loop-body body-exprs)
            (if (null? body-exprs)
                'done
                (if (definition? (car body-exprs))
                        (begin 
                            (set! internal-def-names (cons (definition-variable (car body-exprs))
                                                        internal-def-names 
                                                    )
                            )
                            (set! internal-def-values (cons (definition-value (car body-exprs))
                                                            internal-def-values                           
                                                    )
                            )
                        )
                        (set! other-body-exprs (append other-body-exprs (list (car body-exprs))))
                )
                (loop-body (cdr body-exprs))
            )
        )
        (loop-body proc-body)
        (make-let (map (lambda (x) (list x '*unassigned)) internal-def-names)
                  (append (map (lambda (x y) (list 'set! x y)) internal-def-names internal-def-values) 
                          other-body-exprs
                  )
        )
    )      
)

#| c. |#

(define (make-procedure parameters body env)
    (list 'procedure parameters (scan-out-defines body) env)
)

#| I would rather put it in make-procedure so that it's done once
for all rather than having to repeatedly call it within procedure
body
|#

#| Exercise 4.17 |#

#| 
There is an extra frame because the define are transforming to a let 
which is interpreted like a lambda, with an extra frame
|#

#| 
The general idea would be to assign the variable in the current
frame instead of making a new one.
Move all defines "unassigned" at the top to prevent
it to build new frames. 
|#

#| Exercise 4.18 |#

#| (Not sure) But I think none would work. |#

#| Exercise 4.19 |#

#| The answer is on the book... |#

#| Exercise 4.20 |#

#| a.  |#

(define (letrec? expr)
    (tagged-list? 'letrec expr)
)

(define (letrec-pairs expr)
    (cadr expr)
)

(define (letrec-variables expr)
    (map car (letrec-pairs expr))    
)

(define (letrec-values expr)
    (map cadr (letrec-pairs expr))
)

(define (letrec-body expr)
    (caadr expr)
)

(define (letrec->let expr)
    (make-let 
        (map (lambda (x) (list x '*unassigned)) (letrec-variables expr))
        (append (map (lambda (x y) (list 'set x y)) 
                     (letrec-variables expr) 
                     (letrec-values expr)
                )
                (letrec-body expr)
        )
    )
)

#| b. |#

#| 
I had the correct version, but check 
http://community.schemewiki.org/?sicp-ex-4.20
that is cleaner
|#

#| Exercise 4.21 |#

#| Similar procedure for computing fiboncacci numbers (naive) |#

(lambda (n)
    ((lambda (fib) (fib fib n))
     (lambda (fibo n)
             (if (< n 2)
                 n
                 (else (+ (fibo fibo (- n 1)) (fibo fibo (- n 2))))
             )
    )
   )
)

#| Similar procedure for computing fibonacci numbers |#

(lambda (n)
    ((lambda (fib)
        (if (< n 2)
            n
            (fib fib 0 1 (- n 1))
        )
     )
     (lambda (fibo a b count)
        (if (= 0 count)
            b
            (fibo fibo b (+ a b) (- count 1))
        )
     )
    )
)

#| b. |#

(define (f x)
    ((lambda (even? odd?)
        (even? even? odd? x)
     )
     (lambda (ev? od? n)
        (if (= n 0) 
            #t
            (od? ev? od? (- n 1))
        )
     )
     (lambda (ev? od? n)
        (if (= n 0)
            #f
            (ev? ev? od? (- n 1))
        )
     )
    )
)

#| Exercise 4.22 |#

(define (analyze-let exp)
    (let ((vars (analyze (vars exp)))
          (eprocs (map analyze (exprs exp)))
          (bproc (analyze-sequence (body exp)))
         )
         (lambda (env) (execute-application
                        (make-procedure vars bproc env)
                        (map (lambda (eproc) (eproc env))
                              eprocs
                        )
        ))      
    )
)

#| or just add ((let? expr) (analyze (let->combination expr))) in analyze |#

#| Exercise 4.23 |#

#| 1 exprs |#

#| 
book => (analyze first-proc)
Alyssa's => (lambda (env) (first-proc env))

=> Extra function 
|#

#| See discussion here http://community.schemewiki.org/?sicp-ex-4.23 |#


#| Exercice 4.24 |#

#| Won't do |#

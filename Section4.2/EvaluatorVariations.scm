(define (unless condition usual-value exceptional-value)
    (if condition exceptional-value usual-value)
)

#| The evaluation clause of eval becomes |#

((application? exp)
 (apply (actual-value (operator exp) env)
        (operands exp)
        env
 )
)

(define (actual-value exp env)
    (force-it (eval exp env))
)

(define (apply procedure arguments env)
        (cond ((primitive-procedure? procedure) 
                (apply-primitive-procedure procedure
                                          (list-of-arg-values arguments env)
               )
               ((compound-procedure? procedure)
                (eval-sequence (procedure-body procedure)
                           (extend-environment
                                   (procedure-parameters procedure)
                                   (list-of-delayed-args arguments env)
                                   (procedure-environment procedure)
                           )
                )
               )
        (else (error "Unknown procedure type -- APPLY" procedure))
    )
)

(define (list-of-arg-values exps env)
    (if (no-operands? exps)
        '()
        (cons (actual-value (first-operand exps) env))
            (list-of-args-value (rest-operands exps) env)
        )
    )
)

(define (list-of-delayed-args exps env)
    (if (no-operands? exps)
    '()
    (cons (delay-it (first-operand exps) env))
        (list-of-delayed-args (rest-operands exps) env)
    )
)

(define (eval-if exp env)
    (if (true? (actual-value (if-predicate exp) env))
        (eval (if-consequent exp) env)
        (eval (if-alternative exp) env)
    )
)

(define input-prompt "::: L-Eval input: ")

(define output-prompt "::: L-eval value:")

(define (driver-loop)
    (prompt-for-input input-prompt)
    (let ((input (read)))
        (if (not (eq? input 'exit))
            (begin (let ((output (actual-value input the-global-environment)))
                (announce-output output-prompt)
                (user-print output)
            )
            (driver-loop))
        )
    )
)

(define (force-it obj)
    (if (thunk? obj)
        (actual-value (thunk-exp obj) (thunk-env obj))
        obj
    )
)

(define (delay-it exp env)
    (list 'thunk exp env)
)

(define (thunk? obj)
    (tagged-list? obj 'thunk)
)

(define (thunk-exp obj)
    (cadr obj)
)

(define (thunk-env obj)
    (caddr obj)
)


(define (evaluated-thunk? obj)
    (tagged-list? obj 'evaluated-thunk)
)

(define (thunk-value evaluated-thunk)
    (cadr evaluated-thunk)
)

(define (force-it obj)
    (cond ((thunk? obj)
           (let ((result (actual-value  
                            (thunk-exp obj)
                            (thunk-env obj)
                )))
                (set-car! obj 'evaluated-thunk)
                (set-car! (cdr obj) result)
                (set-cdr! (cdr obj) '())
                result
           )
          )
          ((evaluated-thunk? obj)
           (thunk-value obj)
          )
          (else obj)
    )
)

(define (cons x y)
    (lambda (m) (m x y))
)

(define (car z)
    (z (lambda (p q) p))
)

(define (cdr z)
    (z (lambda (p q) q))
)

(define (list-ref items n)
    (if (= n 0)
        (car items)
        (list-ref (cdr items) (- n 1))
    )
)


(define (map proc items)
    (if (null? items)
        '()
        (cons (proc (car items))
              (map proc (cdr items))
        )
    )
)

(define (scale-list items factor)
    (map (lambda (x) (* x factor)) items)
)

(define (add-lists list1 list2)
    (cond ((null? list1) list2)
          ((null? list2) list1)
          (else (cons (+ (car list1) (car list2))
                      (add-lists (cdr list1) (cdr list2))
                )
          )
    )
)

(define ones (cons 1 ones))

(define integers 
    (cons 1
          (add-lists ones integers)
    )
)

(define (integral integrand initial-value dt)
    (define int
        (cons initial-value
              (add-lists (scale-list integrand dt)
                         int
              )
        )
    )
    int
)

(define (solve f y0 dt)
    (define y (integral dy y0 dt))
    (define dy (map f y))
)


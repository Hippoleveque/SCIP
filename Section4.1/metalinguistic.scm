(define (eval expr env)
    (cond ((self-evaluating? expr) expr)
          ((variable? expr) (lookup-variable-value expr env))
          ((quoted? expr) (text-of-quotation expr))
          ((assignment? expr) (eval-assignment expr env))
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

(define (my-apply procedure arguments)
    (cond ((primitive-procedure? procedure) 
            (apply-primitive-procedure procedure arguments))
          ((compound-procedure? procedure)
           (eval-sequence (procedure-body procedure)
                          (extend-environment
                                (procedure-parameters procedure)
                                arguments
                                (procedure-environment procedure)
                          )
           ))
           (else (error "Unknown procedure type -- APPLY" procedure))
    )
)

(define (list-of-values exps env)
    (if (no-operands? exps)
        '()
        (cons (eval (first-operand exps) env)
              (list-of-values (rest-operands exps) env)
        )
    )
)


(define (eval-if exp env)
    (if (true? (eval (if-predicate exp) env))
        (eval (if-consequent exp) env)
        (eval (if-alternative exp) env)
    )
)

(define (eval-sequence exps env)
    (cond ((last-exp? exps) (eval (first-exp exps) env))
          (else (eval (first-exp exps) env)
                (eval-sequence (rest-exps exps) env)
          )
    )
)

(define (eval-assignment exp env)
    (set-variable-value! (assignment-variable exp)
                         (eval (assignment-value exp) env)
                          env
    )
    'ok
)

(define (eval-definition exp env)
    (define-variable! (definition-variable exp)
                      (eval (definition-value exp) env) 
                      env
    )
    'ok
)

(define (self-evaluating? expr)
    (cond ((number? expr) #t)
          ((string? expr) #t)
          (else #f)
    )
)

(define (variable? expr)
    (symbol? expr)
)

(define (quoted? expr)
    (tagged-list? expr 'quote)
)

(define (text-of-quotation expr)
    (cadr expr)
)

(define (tagged-list? expr tag)
    (if (pair? expr)
        (eq? (car expr) tag)
        #f
    )
)

(define (assignment? expr)
    (tagged-list? expr 'set!)
)

(define (assignment-variable expr)
    (cadr expr)
)

(define (assignment-value expr)
    (caddr expr)
)

(define (definition? expr)
    (tagged-list? expr 'define)
)

(define (definition-variable expr)
    (if (symbol? (cadr expr))
        (cadr expr)
        (caadr expr)
    )
)

(define (definition-value expr)
    (if (symbol? (cadr expr))
        (caddr expr)
        (make-lambda (cdadr expr)
                     (cddr expr) ; Because it could be a sequence of expressions
        )
    )
)

(define (lambda? expr)
    (tagged-list? expr 'lambda)
)

(define (lambda-parameters expr)
    (cadr expr)
)

(define (lambda-body expr)
    (cddr expr) ; Because it could be a sequence of expressions
)

(define (make-lambda parameters body) ; Be careful: Body needs to be a list of expressions
    (cons 'lambda (cons parameters body))
)

(define (if? expr)
    (tagged-list? expr 'if)
)

(define (if-predicate expr)
    (cadr expr)
)   

(define (if-consequent expr)
        (caddr expr)
)

(define (if-alternative expr)
    (if (null? (cdddr expr))
        #f
        (cadddr expr) ; Be careful: we consider the alternative to be a single expression
    )
)   

(define (make-if predicate consequent alternative) ; Be careful: we consider the alternative to be a single expression
    (list 'if predicate consequent alternative)
)   

(define (begin? expr)
    (tagged-list? expr 'begin)
)

(define (begin-actions expr)
    (cdr expr)
)

(define (last-exp? seq)
    (null? (cdr seq))
)

(define (first-exp seq)
    (car seq)
)

(define (rest-exps seq)
    (cdr seq)
)

(define (sequence->exp seq)
    (cond ((null? seq) seq)
          ((last-exp? seq) (first-exp seq))
          (else (make-begin seq))
    )
)

(define (make-begin seq)
    (cons 'begin seq)
)

(define (application? expr) (pair? expr))

(define (operator expr) (car expr))

(define (operands expr) (cdr expr))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

(define (cond? expr)
    (tagged-list? expr 'cond)
)

(define (cond-clauses expr)
    (cdr expr)
)

(define (cond-else-clause? clause)
    (tagged-list? (cond-predicate clause) 'else)
)

(define (cond-predicate clause)
    (car clause)
)

(define (cond-actions clause)
    (cdr clause) ; Be careful, the clause here is considered to be a sequence
)

(define (cond->if expr)
    (expand-clauses (cond-clauses expr))
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
                    (error "ELSE clause isn't last -- COND->IF" clauses)
                )
                (make-if (cond-predicate first)
                    (sequence->exp (cond-actions first))
                    (expand-clauses rest)
                )
            )    
        )
    )
)

(define (true? x)
    (not (eq? x #f))
)

(define (false? x)
    (eq? x #f)
)

(define (make-procedure parameters body env)
    (list 'procedure parameters body env)
)

(define (compound-procedure? p)
    (tagged-list? p 'procedure)
)

(define (procedure-parameters p)
    (cadr p)
)

(define (procedure-body p)
    (caddr p)
)

(define (procedure-environment p)
    (cadddr p)
)

(define (enclosing-environment env)
    (cdr env)
)

(define (first-frame env)
    (car env)
)

(define the-empty-environment '())

(define (make-frame variables values)
    (cons variables values)
)

(define (frame-variables frame)
    (car frame)
)

(define (frame-values frame)
    (cdr frame)
)

(define (add-binding-to-frame! var val frame)
    (set-car! frame (cons var (frame-variables frame)))
    (set-cdr! frame (cons val (frame-values frame)))
)

(define (extend-environment vars vals base-env)
    (if (= (length vars) (length vals))
        (cons (make-frame vars vals) base-env)
        (if (> (length vars) (length vals))
            (error "Too many arguments supplied" vars vals)
            (error "Too few arguments supplieds" vars vals)
        )
    )
)


(define (lookup-variable-value var env)
    (define (env-loop env)
        (define (scan vars vals)
            (cond ((null? vars) (env-loop (enclosing-environment env)))
                  ((eq? var (car vars)) (car vals))
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

(define (set-variable-value! var val env)
    (define (env-loop env)
        (define (scan vars vals)
            (cond ((null? vars) (env-loop (enclosing-environment env)))
                ((eq? var (car vars)) (set-car! vals val))
                (else (scan (cdr vars) (cdr vals)))
            )
        )
        (if (eq? env the-empty-environment)
            (error "Unbound variable --SET" var)
            (let ((frame (first-frame env)))
                (scan (frame-variables frame) (frame-values frame))
            )
        )
    )
    (env-loop env)
)

(define (define-variable! var val env)
    (let ((frame (first-frame env)))
        (define (scan vars vals)
            (cond ((null? vars) (add-binding-to-frame! var val frame))
                  ((eq? (car vars) var)
                   (set-car! vals val)
                  )
                  (else (scan (cdr vars) (cdr vals)))
            )
        )
        (scan (frame-variables frame) (frame-values frame))
    )
)

(define (setup-environment)
    (let ((initial-environment (extend-environment 
                                    (primitive-procedure-names)
                                    (primitive-procedure-objects)
                                    the-empty-environment
                              )
          ))
          (define-variable! 'true #t initial-environment)
          (define-variable! 'false #f initial-environment)
          initial-environment
    )
)

(define the-global-environment (setup-environment))

(define (primitive-procedure? proc)
    (tagged-list? proc 'primitive)
)

(define (primitive-implementation proc)
    (cadr proc)
)

(define primitive-procedures
    (list (list 'car car)
          (list 'cdr cdr)
          (list 'cons cons)
          (list 'null? null?)
    )
)

(define (primitive-procedure-names)
    (map car primitive-procedures)
)

(define (primitive-procedure-objects)
    (map (lambda (proc) (list 'primitive (cadr proc))) primitive-procedures)
)

(define (apply-primitive-procedure proc args)
    (apply-in-underlying-scheme (primitive-implementation proc) args)
)

(define apply-in-underlying-scheme apply)

(define input-prompt "::: M-Eval input:")
(define output-prompt "::: M-Eval value:")

(define (driver-loop)
    (prompt-for-input input-prompt)
    (let ((input (read)))
        (if (not (eq? input 'exit))
            (begin (let ((output (eval input the-global-environment)))
                (announce-output output-prompt)
                (user-print output)
            )
            (driver-loop))
        )
    )
)

(define (prompt-for-input string)
    (newline)
    (newline)
    (display string)
    (newline)
)

(define (announce-output string)
    (newline)
    (display string)
    (newline)
)

(define (user-print object)
    (if (compound-procedure? object)
        (display (list 'compound-procedure
                       (procedure-parameters object)
                       (procedure-body object)
                        '<procedure-env>
                 )
        )
        (display object)
    )
)

(define (eval exp env)
    ((analyze exp) env)
)

(define (analyze exp)
    (cond ((self-evaluating? exp) 
           (analyze-self-evaluating exp))
          ((quoted? exp)
           (analyze-quoted exp)
          )
          ((variable? exp)
           (analyze-variable exp)
          )
          ((assignment? exp)
           (analyze-assignment exp)
          )
         ((definition? exp)
          (analyze-definition exp)
         )
         ((if? exp)
          (analyze-if exp)
         )
         ((lambda? exp)
          (analyze-lambda exp)
         )
         ((begin? exp)
          (analyze-sequence (begin-actions exp))
         )
         ((cond? exp)
          (analyze (cond->if exp))
         )
         ((application? exp)
          (analyze-application exp)
         )
         (else (error "Unknown expression type -- ANALYZE" exp))
    )
)

(define (analyze-self-evaluating exp)
    (lambda (env) exp)
)

(define (analyze-quoted exp)
    (let ((qval (text-of-quotation exp)))
        (lambda (env) qval)
    )
)

(define (analyze-variable exp)
    (lambda (env) (lookup-variable-value exp env))
)

(define (analyze-assignment exp)
    (let ((var (assignment-variable exp))
          (vproc (analyze (assignment-value exp)))
         )
         (lambda (env) (set-variable-value! var (vproc env) env))
    )
)

(define (analyze-definition exp)
    (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp)))
        )
        (lambda (env) (define-variable! var (vproc env) env))
    )
)

(define (analyze-if exp)
    (let ((pproc (analyze (if-predicate exp)))
          (cproc (analyze (if-consequent exp)))
          (aproc (analyze (if-alternative exp)))
         )
         (lambda (env) 
                 (if (true? (pproc env))
                     (cproc env)
                     (aproc env)
                 )
        )
    )
)

(define (analyze-lambda exp)
    (let ((vars (analyze (lambda-parameters exp)))
          (bproc (analyze-sequence (lambda-body exp)))
        )
        (lambda (env) (make-procedure vars bproc env))
    )
)


(define (analyze-sequence exp)
    (define (sequentially proc1 proc2)
        (lambda (env) (proc1 env) (proc2 env))
    )
    (define (loop first-proc rest-procs)
        (if (null? rest-procs)
            first-proc
            (loop (sequentially first-proc (car rest-procs)) 
                  (cdr rest-procs)
            )
        )
    )
    (let ((procs (map analyze exp)))
        (if (null? procs)
            (error "Empty sequence -- ANALYZE")
            (loop (car procs) (cdr procs))
        )
    )
)

(define (analyze-application exp)
    (let ((fproc (analyze (operator exp)))
          (aprocs (map analyze (operands exp)))
         )
         (lambda (env) 
                 (execute-application (fproc env) 
                                      (map (lambda (aproc) (aproc env)) aprocs)
                 )
         )
    )
)

(define (execute-application proc args)
    (cond ((primitive-procedure? proc) 
           (apply-primitive-procedure proc args)
          )
          ((compound-procedure? proc)
           ((procedure-body proc)
            (extend-environment 
                (procedure-parameters proc)
                args
                (procedure-environment proc)
            )
           )
          )
          (else (error "Unknown procedure type -- Execute-Application" proc))

    )
)

#| Exercise 5.31 |#

#| a. |#

(f 'x 'y)

#| 
1. Save and restore env around the
evaluator is useless as it's just 
a lookup (no application evaluation is
done)

2. Saving and restoring the env around 
the evaluation of each operand is
useless as well because they are 
directly evaluated

3. Saving argl is also useless 
as no operand needs any argl

4. Save and restore proc is usefull because
the proc will host f and (f)

|#

#| b. |#

((f) 'x 'y)

#| 
1. Useless 


|#

#| c. |#

(f (g 'x) y)

#| 
1. Useless

2. It is useful around the evaluation
of the first operand as it will create
a new environment

3. Useful because the argl will be used in the
evaluation of the first operand

4. Usefull because the proc register
will be used in the evaluation
of the first operand

|#

#| d. |#

(f (g 'x) 'y)

#| 
1. Useless

2. Useless, 'x and 'y won't be affected by any env change 
in the evaluation of (g 'x)

3. usefull 

4. useful

|#

#| Exercise 5.32 |#

#| a. |#

(define eceval
    (make-machine
        '(exp env 
     proc argl continue unev)
        eceval-operations
        '(read-eval-print-loop
            (perform (op initialize-stack))
            (perform 
                (op prompt-for-input)
                (const "::: EC-Eval input: ")
            )
            (assign exp (op read))
            (assign env (op get-global-environment))
            (assign continue (label print-result))
            (goto (label eval-dispatch))

        eval-dispatch
            (test (op self-evaluating?) (reg exp))
            (branch (label ev-self-eval))
            (test (op variable?) (reg exp))
            (branch (label ev-variable))
            (test (op quoted?) (reg exp))
            (branch (label ev-quoted))
            (test (op assignment?) (reg exp))
            (branch (label ev-assignment))
            (test (op definition?) (reg exp))
            (branch (label ev-definition))
            (test (op if?) (reg exp))    
            (branch (label ev-if))
            (test (op lambda?) (reg exp))
            (branch (label ev-lambda))
            (test (op begin?) (reg exp))
            (branch (label ev-begin))
            (test (op application?) (reg exp))
            (branch (label ev-application))
            (goto (label unknown-expression-type))

        ev-self-eval
            (assign 
             (reg exp))
            (goto (reg continue))

        ev-variable
            (assign 
             (op lookup-variable-value) (reg exp) (reg env))
            (goto (reg continue))

        ev-quoted
            (assign 
             (op text-of-quotation) (reg exp))
            (goto (reg continue))

        ev-lambda 
            (assign unev (op lambda-parameters) (reg exp))
            (assign exp (op lambda-body) (reg exp))
            (assign 
             (op make-procedure) (reg unev) (reg exp) (reg env))
            (goto (reg continue))

        ev-application
            (save continue)
            (assign unev (op operands) (reg exp))
            (assign exp (op operator) (reg exp))
            (test (op is-symbol-operator?) (reg exp))
            (branch (label ev-appl-continue-symbol-operator))
            (save env)
            (save unev)
            (assign continue (label ev-appl-did-operator))
            (goto (label eval-dispatch))
    
        ev-appl-continue-symbol-operator
            (assign continue (label ev-appl-did-symbol-operator))
            (goto (label eval-dispatch))

        ev-appl-did-symbol-operator
            (assign argl (op empty-arglist))
            (assign proc (reg 
            ))
            (test (op no-operands?) (reg unev))
            (branch (label apply-dispatch))
            (save proc)
            (goto (label ev-appl-operand-loop))

        ev-appl-did-operator
            (restore unev)
            (restore env)
            (assign argl (op empty-arglist))
            (assign proc (reg 
            ))
            (test (op no-operands?) (reg unev))
            (branch (label apply-dispatch))
            (save proc)

        ev-appl-operand-loop
            (save argl)
            (assign exp (op first-operand) (reg unev))
            (test (op last-operand?) (reg unev))
            (branch (label ev-appl-last-arg))
            (save env)
            (save unev)
            (assign continue (label ev-appl-accumulate-arg))
            (goto (label eval-dispatch))

        ev-appl-accumulate-arg
            (restore unev)
            (restore env)
            (restore argl)
            (assign argl (op adjoin-arg) (reg 
            ) (reg argl))
            (assign unev (op rest-operands) (reg unev))
            (goto (label ev-appl-operand-loop))

        ev-appl-last-arg
            (assign continue (label ev-appl-accum-last-arg))
            (goto (label eval-dispatch))

        ev-appl-accum-last-arg
            (restore argl)
            (assign argl (op adjoin-arg) (reg 
            ) (reg argl))
            (restore proc)
            (goto (label apply-dispatch))

        apply-dispatch
            (test (op primitive-procedure?) (reg proc))
            (branch (label primitive-apply))
            (test (op compound-procedure?) (reg proc))
            (branch (label compound-apply))
            (goto (label unknown-procedure-type))

        primitive-apply
            (assign 
             (op apply-primitive-procedure) (reg proc) (reg argl))
            (restore continue)
            (goto (reg continue))

        compound-apply
            (assign unev (op procedure-parameters) (reg proc))
            (assign env (op procedure-environment) (reg proc))
            (assign env (op extend-environment) (reg unev) (reg argl) (reg env))
            (assign unev (op procedure-body) (reg proc))
            (goto (label ev-sequence))

        ev-begin 
            (assign unev (op begin-actions) (reg exp))
            (save continue)
            (goto (label ev-sequence))

        ev-sequence
            (assign exp (op first-exp) (reg unev))
            (test (op last-exp?) (reg unev))
            (branch (label ev-sequence-last-exp))
            (save unev)
            (save env)
            (assign continue (label ev-sequence-continue))
            (goto (label eval-dispatch))

        ev-sequence-continue
            (restore env)
            (restore unev)
            (assign unev (op rest-exps) (reg unev))
            (goto (label ev-sequence))

        ev-sequence-last-exp
            (restore continue)
            (goto (label eval-dispatch))

        ev-if
            (save exp)
            (save env)
            (save continue)
            (assign continue (label ev-if-decide))
            (assign exp (op if-predicate) (reg exp))
            (goto (label eval-dispatch))

        ev-if-decide
            (restore continue)
            (restore env)
            (restore exp)
            (test (op true?) (reg 
            ))
            (branch (label ev-if-consequent))

        ev-if-alternative
            (assign exp (op if-alternative) (reg exp))
            (goto (label eval-dispatch))

        ev-if-consequent
            (assign exp (op if-consequent) (reg exp))
            (goto (label eval-dispatch))

        ev-assignment
            (assign unev (op assignment-variable) (reg exp))
            (save unev)
            (assign exp (op assignment-value) (reg exp))
            (save env)
            (save continue)
            (assign continue (label ev-assignment-1))
            (goto (label eval-dispatch))

        ev-assignment-1
            (restore continue)
            (restore env)
            (restore unev)
            (perform (op set-variable-value!) (reg unev) (reg 
            ) (reg env))
            (assign 
             (const ok))
            (goto (reg continue))

        ev-definition
            (assign unev (op definition-variable) (reg exp))
            (save unev)
            (assign exp (op definition-value) (reg exp))
            (save env)
            (save continue)
            (assign continue (label ev-definition-1))
            (goto (label eval-dispatch))

        ev-definition-1
            (restore continue)
            (restore env)
            (restore unev)
            (perform (op define-variable!) (reg unev) (reg 
            ) (reg env))
            (assign 
             (const ok))
            (goto (reg continue))

        print-result
            (perform (op print-stack-statistics))
            (perform
                (op announce-output)
                (const "::: EC-Eval value:")
            )
            (perform (op user-print) (reg 
            ))
            (goto (label read-eval-print-loop))

        unknown-expression-type
            (assign 
             (const unknown-expression-type-error))
            (goto (label signal-error))

        unknown-procedure-type
            (restore continue)
            (assign 
             (const unknown-procedure-type-error))
            (goto (label signal-error))

        signal-error
            (perform (op user-print) (reg 
            ))
            (goto (label read-eval-print-loop))
        )
    )
)

(define eceval-operations
    (list (list 'self-evaluating? self-evaluating?)
        (list 'variable? variable?)
        (list 'quoted? quoted?)
        (list 'assignment? assignment?)
        (list 'definition? definition?)
        (list 'if? if?)
        (list 'lambda? lambda?)
        (list 'begin? begin?)
        (list 'application? application?)
        (list 'lookup-variable-value lookup-variable-value)
        (list 'text-of-quotation text-of-quotation)
        (list 'lambda-parameters lambda-parameters)
        (list 'lambda-body lambda-body)
        (list 'make-procedure make-procedure)
        (list 'operands operands)
        (list 'operator operator)
        (list 'empty-arglist empty-arglist)
        (list 'no-operands? no-operands?)
        (list 'first-operand first-operand)
        (list 'last-operand? last-operand?)
        (list 'adjoin-arg adjoin-arg)
        (list 'rest-operands rest-operands)
        (list 'primitive-procedure? primitive-procedure?)
        (list 'compound-procedure? compound-procedure?)
        (list 'apply-primitive-procedure apply-primitive-procedure)
        (list 'procedure-parameters procedure-parameters)
        (list 'procedure-environment procedure-environment)
        (list 'extend-environment extend-environment)
        (list 'procedure-body procedure-body)
        (list 'begin-actions begin-actions)
        (list 'first-exp first-exp)
        (list 'last-exp? last-exp?)
        (list 'rest-exps rest-exps)
        (list 'if-predicate if-predicate)
        (list 'true? true?)
        (list 'if-alternative if-alternative)
        (list 'if-consequent if-consequent)
        (list 'assignment-variable assignment-variable)
        (list 'assignment-value assignment-value)
        (list 'set-variable-value! set-variable-value!)
        (list 'definition-variable definition-variable)
        (list 'definition-value definition-value)
        (list 'define-variable! define-variable!)
        (list 'announce-output announce-output)
        (list 'user-print user-print)
        (list 'prompt-for-input prompt-for-input)
        (list 'read read)
        (list 'get-global-environment get-global-environment)
        (list 'is-symbol-operator? is-symbol-operator?)
        (list 'print print)
        (list '= =)
        (list '* *)
        (list '+ +)
    )
)

(define (is-symbol-operator? operator)
    (symbol? operator)
)

(define (print x)
    (newline)
    (display x)
    (newline)
)


(define (fact n)
    (if (= n 1)
        1
        (* (fact (- n 1)) n)
    )
)

#| b. |#

#| 
I think Alyssa misses the point because the main advantage 
of compilation is to have the generation of the machine-like
code as a pre-processing step (before starting the program)
so that it is not generated at runtime. 
 
|#

#| Exercise 5.33 |#

(define (factorial n)
    (if (= n 1)
        1
    (* (factorial (- n 1)) n)))

#| 
(assign 
 (op make-compiled-procedure) (label entry5) (reg env))
(goto (label after-lambda4)) 
entry5 
    (assign env (op compiled-procedure-env) (reg proc)) 
    (assign env (op extend-environment) (const (n)) (reg argl) (reg env)) 
    (save continue) 
    (save env) 
    (assign proc (op lookup-variable-value) (const =) (reg env)) 
    (assign 
     (const 1)) (assign argl (op list) (reg 
    )) 
    (assign 
     (op lookup-variable-value) (const n) (reg env)) 
    (assign argl (op cons) (reg 
    ) (reg argl)) 
    (test (op primitive-procedure?) (reg proc)) 
    (branch (label primitive-branch20)) 

compiled-branch19 
    (assign continue (label after-call18)) 
    (assign 
     (op compiled-procedure-entry) (reg proc)) 
    (goto (reg 
    )) 
    
primitive-branch20 
    (assign 
     (op apply-primitive-procedure) (reg proc) (reg argl)) 

after-call18 
    (restore env) 
    (restore continue) 
    (test (op false?) (reg 
    )) 
    (branch (label false-branch7)) 
    
true-branch8 
    (assign 
     (const 1)) 
    (goto (reg continue)) 
    
false-branch7 
    (assign proc (op lookup-variable-value) (const *) (reg env)) 
    (save continue) 
    (save proc) 
    (assign 
     (op lookup-variable-value) (const n) (reg env)) 
    (assign argl (op list) (reg 
    )) 
    (save argl) 
    (assign proc (op lookup-variable-value) (const factorial) (reg env)) 
    (save proc) 
    (assign proc (op lookup-variable-value) (const -) (reg env)) 
    (assign 
     (const 1)) 
    (assign argl (op list) (reg 
    )) 
    (assign 
     (op lookup-variable-value) (const n) (reg env)) 
    (assign argl (op cons) (reg 
    ) (reg argl)) 
    (test (op primitive-procedure?) (reg proc)) 
    (branch (label primitive-branch11)) 
    
compiled-branch10 
    (assign continue (label after-call9)) 
    (assign 
     (op compiled-procedure-entry) (reg proc)) 
    (goto (reg 
    )) 
    
primitive-branch11 
    (assign 
     (op apply-primitive-procedure) (reg proc) (reg argl)) 
    
after-call9 
    (assign argl (op list) (reg 
    )) 
    (restore proc) 
    (test (op primitive-procedure?) (reg proc)) 
    (branch (label primitive-branch14)) 
    
compiled-branch13 
    (assign continue (label after-call12)) 
    (assign 
     (op compiled-procedure-entry) (reg proc)) 
    (goto (reg 
    )) 
    
primitive-branch14 
    (assign 
     (op apply-primitive-procedure) (reg proc) (reg argl))

after-call12 
    (restore argl) 
    (assign argl (op cons) (reg 
    ) (reg argl)) 
    (restore proc) 
    (restore continue) 
    (test (op primitive-procedure?) (reg proc)) 
    (branch (label primitive-branch17)) 
    
compiled-branch16 
    (assign 
     (op compiled-procedure-entry) (reg proc)) 
    (goto (reg 
    )) 
    
primitive-branch17 (
    assign 
 (op apply-primitive-procedure) (reg proc) (reg argl)) 
    (goto (reg continue)) 
    
after-call15 

after-if6 

after-lambda4 
    (perform (op define-variable!) (const factorial) (reg 
    ) (reg env)) 
    (assign 
     (const ok)))

|#

(define (factorial n)
    (if (= n 1)
        1
    (* n (factorial (- n 1)))))


#| 


((assign 
 (op make-compiled-procedure) (label entry22) (reg env)) 
(goto (label after-lambda21)) 
entry22 
    (assign env (op compiled-procedure-env) (reg proc)) 
    (assign env (op extend-environment) (const (n)) (reg argl) (reg env)) 
    (save continue) 
    (save env) 
    (assign proc (op lookup-variable-value) (const =) (reg env)) 
    (assign 
     (const 1)) (assign argl (op list) (reg 
    )) 
    (assign 
     (op lookup-variable-value) (const n) (reg env)) 
    (assign argl (op cons) (reg 
    ) (reg argl)) 
    (test (op primitive-procedure?) (reg proc)) 
    (branch (label primitive-branch37)) 

compiled-branch36 
    (assign continue (label after-call35)) 
    (assign 
     (op compiled-procedure-entry) (reg proc)) 
    (goto (reg 
    )) 

primitive-branch37 
    (assign 
     (op apply-primitive-procedure) (reg proc) (reg argl)) 
    
after-call35 
    (restore env) 
    (restore continue) 
    (test (op false?) (reg 
    )) 
    (branch (label false-branch24)) 

true-branch25 
    (assign 
     (const 1)) 
    (goto (reg continue)) 

false-branch24 
    (assign proc (op lookup-variable-value) (const *) (reg env)) 
    (save continue) 
    (save proc) 
    (save env) 
    (assign proc (op lookup-variable-value) (const factorial-alt) (reg env)) 
    (save proc) 
    (assign proc (op lookup-variable-value) (const -) (reg env)) 
    (assign 
     (const 1)) 
    (assign argl (op list) (reg 
    )) 
    (assign 
     (op lookup-variable-value) (const n) (reg env)) 
    (assign argl (op cons) (reg 
    ) (reg argl)) 
    (test (op primitive-procedure?) (reg proc)) 
    (branch (label primitive-branch28)) 

compiled-branch27 
    (assign continue (label after-call26)) 
    (assign 
     (op compiled-procedure-entry) (reg proc)) 
    (goto (reg 
    )) 
    
primitive-branch28 
    (assign 
     (op apply-primitive-procedure) (reg proc) (reg argl)) 

after-call26 
    (assign argl (op list) (reg 
    )) 
    (restore proc) 
    (test (op primitive-procedure?) (reg proc)) 
    (branch (label primitive-branch31)) 
    
compiled-branch30 
    (assign continue (label after-call29)) 
    (assign 
     (op compiled-procedure-entry) (reg proc)) 
    (goto (reg 
    )) 
    
primitive-branch31 
    (assign 
     (op apply-primitive-procedure) (reg proc) (reg argl)) 
    
after-call29 
    (assign argl (op list) (reg 
    )) 
    (restore env) 
    (assign 
     (op lookup-variable-value) (const n) (reg env)) 
    (assign argl (op cons) (reg 
    ) (reg argl)) 
    (restore proc) 
    (restore continue) 
    (test (op primitive-procedure?) (reg proc)) 
    (branch (label primitive-branch34)) 
    
compiled-branch33 
    (assign 
     (op compiled-procedure-entry) (reg proc)) 
    (goto (reg 
    )) 
    
primitive-branch34 
    (assign 
     (op apply-primitive-procedure) (reg proc) (reg argl)) 
    (goto (reg continue)) 
    
after-call32 

after-if23 

after-lambda21 
    (perform (op define-variable!) (const factorial-alt) (reg 
    ) (reg env)) 
    (assign alt (const ok))))
|#

#| No a lot to say, check http://community.schemewiki.org/?sicp-ex-5.33 |#

#| Exercise 5.34 |#

(define (factorial n)
    (define (iter product counter)
        (if (> counter n)
            product
            (iter (* product counter) (+ counter 1))
        )
    )
    (iter 1 1)
)

#| 
((assign 
 (op make-compiled-procedure) (label entry43) (reg env)) 
(goto (label after-lambda42)) 
entry43 
    (assign env (op compiled-procedure-env) (reg proc)) 
    (assign env (op extend-environment) (const (n)) (reg argl) (reg env)) 
    (assign 
     (op make-compiled-procedure) (label entry48) (reg env)) 
    (goto (label after-lambda47)) 
    
entry48 
    (assign env (op compiled-procedure-env) (reg proc)) 
    (assign env (op extend-environment) (const (product counter)) (reg argl) (reg env)) 
    (save continue) 
    (save env) 
    (assign proc (op lookup-variable-value) (const >) (reg env)) 
    (assign 
     (op lookup-variable-value) (const n) (reg env)) 
    (assign argl (op list) (reg 
    )) 
    (assign 
     (op lookup-variable-value) (const counter) (reg env)) 
    (assign argl (op cons) (reg 
    ) (reg argl)) 
    (test (op primitive-procedure?) (reg proc)) 
    (branch (label primitive-branch63)) 
    
compiled-branch62 
    (assign continue (label after-call61)) 
    (assign 
     (op compiled-procedure-entry) (reg proc)) 
    (goto (reg 
    )) 
    
primitive-branch63 
    (assign 
     (op apply-primitive-procedure) (reg proc) (reg argl)) 
    
after-call61 
    (restore env) 
    (restore continue) 
    (test (op false?) (reg 
    )) 
    (branch (label false-branch50)) 
    
true-branch51 
    (assign 
     (op lookup-variable-value) (const product) (reg env)) 
    (goto (reg continue)) 
    
false-branch50 
    (assign proc (op lookup-variable-value) (const iter) (reg env)) 
    (save continue) 
    (save proc) 
    (save env) 
    (assign proc (op lookup-variable-value) (const +) (reg env)) 
    (assign 
     (const 1)) 
    (assign argl (op list) (reg 
    )) 
    (assign 
     (op lookup-variable-value) (const counter) (reg env)) 
    (assign argl (op cons) (reg 
    ) (reg argl)) 
    (test (op primitive-procedure?) (reg proc)) 
    (branch (label primitive-branch57)) 
    
compiled-branch56 
    (assign continue (label after-call55)) 
    (assign 
     (op compiled-procedure-entry) (reg proc)) 
    (goto (reg 
    )) 
    
primitive-branch57 
    (assign 
     (op apply-primitive-procedure) (reg proc) (reg argl)) 
    
after-call55 
    (assign argl (op list) (reg 
    )) 
    (restore env) 
    (save argl) 
    (assign proc (op lookup-variable-value) (const *) (reg env)) 
    (assign 
     (op lookup-variable-value) (const counter) (reg env)) 
    (assign argl (op list) (reg 
    )) 
    (assign 
     (op lookup-variable-value) (const product) (reg env)) 
    (assign argl (op cons) (reg 
    ) (reg argl)) 
    (test (op primitive-procedure?) (reg proc)) 
    (branch (label primitive-branch54)) 
    
compiled-branch53 
    (assign continue (label after-call52)) 
    (assign 
     (op compiled-procedure-entry) (reg proc)) 
    (goto (reg 
    )) 
    
primitive-branch54 
    (assign 
     (op apply-primitive-procedure) (reg proc) (reg argl)) 
    
after-call52 
    (restore argl) 
    (assign argl (op cons) (reg 
    ) (reg argl)) 
    (restore proc) 
    (restore continue) 
    (test (op primitive-procedure?) (reg proc)) 
    (branch (label primitive-branch60)) 
    
compiled-branch59 
    (assign 
     (op compiled-procedure-entry) (reg proc)) 
    (goto (reg 
    )) 
    
primitive-branch60 
    (assign 
     (op apply-primitive-procedure) (reg proc) (reg argl)) 
    (goto (reg continue)) 
    
after-call58 

after-if49 

after-lambda47 
    (perform (op define-variable!) (const iter) (reg 
    ) (reg env)) 
    (assign 
     (const ok)) 
    (assign proc (op lookup-variable-value) (const iter) (reg env)) 
    (assign 
     (const 1)) 
    (assign argl (op list) (reg 
    )) 
    (assign 
     (const 1)) 
    (assign argl (op cons) (reg 
    ) (reg argl)) 
    (test (op primitive-procedure?) (reg proc)) 
    (branch (label primitive-branch46)) 
    
compiled-branch45 
    (assign 
     (op compiled-procedure-entry) (reg proc)) 
    (goto (reg 
    )) 

primitive-branch46 
    (assign 
     (op apply-primitive-procedure) (reg proc) (reg argl)) 
    (goto (reg continue)) 
    
after-call44 

after-lambda42 
    (perform (op define-variable!) (const factorial) (reg 
    ) (reg env)) 
    (assign 
     (const ok)))


|#

#| 
Won't spend too much time on this, goto 
http://community.schemewiki.org/?sicp-ex-5.34 

(Reflects previous explanation about tail recursivity)
|#

#| Exercise 5.35 |#

#| 
(assign 
 (op make-compiled-procedure) (label entry16) (reg env))
(goto (label after-lambda-15))

entry16
    (assign env (op compiled-procedure-env) (reg proc))
    (assign env (op extend-environment) (const (x)) (reg argl) (reg env))
    (assign proc (op lookup-variable-value) (const +) (reg env))
    (save continue)
    (save proc)
    (save env)
    (assign proc (op lookup-variable-value) (const g) (reg env))
    (save proc)
    (assign proc (op lookup-variable-value) (const +) (reg env))
    (assign 
     (const 2))
    (assign argl (op list) (reg 
    ))
    (assign 
     (op lookup-variable-value) (const x) (reg env))
    (assign argl (op cons) (reg 
    ) (reg argl))
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch-19))

compiled-branch18
    (assign continue (label after-call17))
    (assign 
     (op compiled-procedure-entry) (reg proc))
    (goto (reg 
    ))

primitive-branch19
    (assign 
     (op apply-primitive-procedure) (reg proc) (reg argl))

after-call17
    (assign argl (op list) (reg 
    ))
    (restore proc)
    (test (op-primitive-procedure?) (reg proc))
    (branch (label primitive-branch22))

compiled-branch21
    (assign continue (label after-call20))
    (assign 
     (op compiled-procedure-entry) (reg proc))
    (goto (reg 
    ))

primitive-branch22
    (assign 
     (op apply-primitive-procedure) (reg proc) (reg argl))

after-call20
    (assign argl (op list) (reg 
    ))
    (restore env)
    (assign 
     (op lookup-variable-value) (const x) (reg env))
    (assign argl (op cons) (reg 
    ) (reg argl))
    (restore proc)
    (restore continue)
    (test (op-primitive-procedure?) (reg proc))
    (branch (label primitive-branch25))

compiled-branch24
    (assign 
     (op compiled-procedure-entry) (reg proc))
    (goto (reg 
    ))

primitive-branch25
    (assign 
     (op apply-primitive-procedure) (reg proc) (reg argl))
    (goto (reg continue))

after-call23

after-lambda15
    (perform (op define-variable!) (const f) (reg 
    ) (reg env))
    (assign 
     (const ok))

|#

(define (f x)
    (+ x (g (+ x 2)))
)

#| Exercise 5.36 |#

#| 
The compiler produce a right-to-left evaluation
of the operands. 
This choice is being made in construct-arglist and code-to-get-rest-args

|#

(define (construct-arglist operand-codes)
    (if (null? operand-codes)
        (make-instruction-sequence 
            '()
            '(argl)
            '((assign argl (const ())))
        )
        (let ((code-to-get-first-arg 
                (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence '(
                    
                ) '(argl)
                    '((assign argl (op list) (reg 
                )))
                )
                )
            ))
            (if (null? (cdr operand-codes))
                code-to-get-first-arg
                (preserving '(env)
                            code-to-get-first-arg 
                            (code-to-get-rest-args
                                (cdr operand-codes)
                            )
                )
            )
        )
    )
)

(define (code-to-get-rest-args operand-codes)
    (let ((code-for-next-arg (preserving '(argl)
                                (car operand-codes)
                                (append-instruction-sequences
                                    (make-instruction-sequence
                                        '(
                                            
                                        )
                                        '(
                                            
                                        )
                                        '((assign 
                                     (op list) (reg 
                                    )))
                                    )
                                    (make-instruction-sequence '(
                                         argl)
                                        '(argl)
                                        '((assign argl
                                            (op append)
                                            (reg argl)
                                            (reg 
                                            )
                                        ))
                                    )
                                )

                            ))
        )
        (if (null? (cdr operand-codes))
            code-for-next-arg
            (preserving '(env)
                        code-for-next-arg
                        (code-to-get-rest-args (cdr operand-codes))
            )
        )
    )
)

#| Exercise 5.37 |#

#| Not very useful |#

#| Exercise 5.38 |#

#| a. |#

(define (spread-arguments operands-list)
    (define (iter op-list regs)
        (if (null? (cdr op-list))
            (compile (car op-list) (car regs) 'next)
            (preserving regs
                    (compile (car op-list) (car regs) 'next)
                    (iter (cdr op-list) (cdr regs))
            )
        )
    )
    (iter operands-list (list '
     'argl))
)

#| b. |#

(define (plus-generator exp target linkage)
    (end-with-linkage linkage
        (append-instruction-sequences
            (spread-arguments (operands exp))
            (make-instruction-sequence
                (list '
                 'argl)
                (list target)
                `((assign ,target (op +) (reg 
            ) (reg argl)))
            )
        )
    )
)

(define (equal-generator exp target linkage)
    (end-with-linkage linkage
        (append-instruction-sequences
            (spread-arguments (operands exp))
            (make-instruction-sequence
                (list '
                 'argl)
                (list target)
                `((assign ,target (op =) (reg 
            ) (reg argl)))
            )
        )
    )
)

(define (mul-generator exp target linkage)
    (end-with-linkage linkage
        (append-instruction-sequences
            (spread-arguments (operands exp))
            (make-instruction-sequence
                (list '
                 'argl)
                (list target)
                `((assign ,target (op *) (reg 
            ) (reg argl)))
            )
        )
    )
)

(define (minus-generator exp target linkage)
    (end-with-linkage linkage
        (append-instruction-sequences
            (spread-arguments (operands exp))
            (make-instruction-sequence
                (list '
                 'argl)
                (list target)
                `((assign ,target (op -) (reg 
            ) (reg argl)))
            )
        )
    )
)

(define (compile exp target linkage)
    (cond ((self-evaluating? exp)
        (compile-self-evaluating exp target linkage)
        )
        ((quoted? exp)
        (compile-quoted exp target linkage)
        )
        ((variable? exp)
        (compile-variable exp target linkage)
        )
        ((is-plus? exp)
        (plus-generator exp target linkage)
        )
        ((is-equal? exp)
        (equal-generator exp target linkage)
        )
        ((is-minus? exp)
        (minus-generator exp target linkage)
        )
        ((is-mul? exp)
        (mul-generator exp target linkage)
        )
        ((assignment? exp)
        (compile-assignment exp target linkage)
        )
        ((definition? exp)
        (compile-definition exp target linkage)
        )
        ((if? exp)
        (compile-if exp target linkage)
        )
        ((lambda? exp)
        (compile-lambda exp target linkage)
        )
        ((begin? exp)
        (compile-sequence (begin-actions exp) target linkage)
        )
        ((cond? exp)
        (compile (cond->if exp) target linkage)
        )
        ((application? exp)
        (compile-application exp target linkage)
        )
        (else 
            (error "Unknown expression type -- COMPILE" exp)
        )
    )
)

(define (is-plus? exp) (tagged-list? exp '+))
(define (is-equal? exp) (tagged-list? exp '=))
(define (is-minus? exp) (tagged-list? exp '-))
(define (is-mul? exp) (tagged-list? exp '*))


#| c.  |#

#| 
(assign 
 (op make-compiled-procedure) (label entry95) (reg env)) 
(goto (label after-lambda94)) 
entry95 
    (assign env (op compiled-procedure-env) (reg proc)) 
    (assign env (op extend-environment) (const (n)) (reg argl) (reg env)) 
    (assign 
     (op lookup-variable-value) (const n) (reg env)) 
    (assign argl (const 1)) 
    (assign 
     (op =) (reg 
    ) (reg argl)) 
    (test (op false?) (reg 
    )) (branch (label false-branch97)) 

true-branch98 
    (assign 
     (const 1)) (goto (reg continue)) 

false-branch97 
    (save continue) 
    (assign proc (op lookup-variable-value) (const factorial) (reg env)) 
    (assign 
     (op lookup-variable-value) (const n) (reg env)) 
    (assign argl (const 1)) (assign 
     (op -) (reg 
    ) (reg argl)) 
    (assign argl (op list) (reg 
    )) (test (op primitive-procedure?) (reg proc)) 
    (branch (label primitive-branch101)) 
    
compiled-branch100 
    (assign continue (label after-call99)) 
    (assign 
     (op compiled-procedure-entry) (reg proc)) 
    (goto (reg 
    )) 
    
primitive-branch101 
    (assign 
     (op apply-primitive-procedure) (reg proc) (reg argl)) 
    
after-call99 
    (assign argl (op lookup-variable-value) (const n) (reg env)) 
    (assign 
     (op *) (reg 
    ) (reg argl)) 
    (restore continue) (goto (reg continue)) 

after-if96 

after-lambda94 
    (perform (op define-variable!) (const factorial) (reg 
    ) (reg env)) 
    (assign 
     (const ok))

|#

#| Clearly this code is really smaller (38 lines vs 96) |#

#| d. |#

(define (spread-arguments-plus operands-list)
    (define (iter op-list regs)
        (cond ((and (null? regs) (null? (cdr op-list)))
                (preserving
                    (list '
                     'argl)
                    (make-instruction-sequence
                        (list '
                         'argl)
                        (list '
                        )
                        `((assign 
                     (op +) (reg 
                    ) (reg argl)))
                    )
                    (compile (car op-list) 'argl 'next)
                ))
                ((null? regs)
                (preserving
                    (list '
                     'argl)
                    (append-instruction-sequences
                        (make-instruction-sequence
                            (list '
                             'argl)
                            (list '
                            )
                            `((assign 
                         (op +) (reg 
                        ) (reg argl)))
                        )
                        (compile (car op-list) 'argl 'next)
                    )
                    (iter (cdr op-list) (cdr regs))
                )
                )
                (else 
                    (append-instruction-sequences
                        (compile (car op-list) (car regs) 'next)
                        (iter (cdr op-list) (cdr regs))
                    )
                )
        )
    )
    (iter operands-list (list '
     'argl))
)

(define (spread-arguments-mul operands-list)
    (define (iter op-list regs)
        (cond ((and (null? regs) (null? (cdr op-list)))
                (preserving
                    (list '
                     'argl)
                    (make-instruction-sequence
                        (list '
                         'argl)
                        (list '
                        )
                        `((assign 
                     (op *) (reg 
                    ) (reg argl)))
                    )
                    (compile (car op-list) 'argl 'next)
                ))
                ((null? regs)
                (preserving
                    (list '
                     'argl)
                    (append-instruction-sequences
                        (make-instruction-sequence
                            (list '
                             'argl)
                            (list '
                            )
                            `((assign 
                         (op *) (reg 
                        ) (reg argl)))
                        )
                        (compile (car op-list) 'argl 'next)
                    )
                    (iter (cdr op-list) (cdr regs))
                )
                )
                (else 
                    (append-instruction-sequences
                        (compile (car op-list) (car regs) 'next)
                        (iter (cdr op-list) (cdr regs))
                    )
                )
                
        )
    )
    (iter operands-list (list '
     'argl))
)

(define (mul-generator exp target linkage)
    (end-with-linkage linkage
        (append-instruction-sequences
            (spread-arguments-mul (operands exp))
            (make-instruction-sequence
                (list '
                 'argl)
                (list target)
                `((assign ,target (op *) (reg 
            ) (reg argl)))
            )
        )
    )
)

(define (plus-generator exp target linkage)
    (end-with-linkage linkage
        (append-instruction-sequences
            (spread-arguments-plus (operands exp))
            (make-instruction-sequence
                (list '
                 'argl)
                (list target)
                `((assign ,target (op +) (reg 
            ) (reg argl)))
            )
        )
    )
)

#| 
Would require further testing for the registers 
saves and restores.
|#

#| Exercise 5.39 |#

(define (lexical-address-lookup address rt-env)
    (let ((frame-offset (car address))
          (var-offset) (cdr address)
         )
         (if (> frame-offset 0)
             (lexical-address-lookup (cons (- frame-offset 1) var-offset) (cdr rt-env))
             (lookup-frame-variable-from-offset var-offset (frame-values (first-frame rt-env)))
         )
    )
)

(define (lookup-frame-variable-from-offset offset values)
    (if (= offset 0)
        (if (eq? (car values) '*unassigned)
            (error "Variable is not assigned yet")
            (car values)
        )
        (lookup-frame-variable-from-offset (- offset 1) (cdr values))
    )
)

(define (lexical-address-set! address value rt-env)
    (let ((frame-offset (car address))
        (var-offset) (cdr address)
        )
        (if (> frame-offset 0)
            (lexical-address-set! (cons (- frame-offset 1) var-offset) value (cdr rt-env))
            (set-frame-variable-from-offset! var-offset value (frame-values (first-frame rt-env)))
        )
    )
)

(define (set-frame-variable-from-offset! offset value values)
    (if (= offset 0)
        (set-car! values value)
        (set-frame-variable-from-offset! (- offset 1) (cdr values))
    )
)

#| Exercise 5.40 |#

(define (compile exp target linkage ct-env)
    (cond ((self-evaluating? exp)
        (compile-self-evaluating exp target linkage)
        )
        ((quoted? exp)
        (compile-quoted exp target linkage)
        )
        ((variable? exp)
        (compile-variable exp target linkage ct-env)
        )
        ((assignment? exp)
        (compile-assignment exp target linkage ct-env)
        )
        ((definition? exp)
        (compile-definition exp target linkage)
        )
        ((if? exp)
        (compile-if exp target linkage ct-env)
        )
        ((lambda? exp)
        (compile-lambda exp target linkage ct-env)
        )
        ((begin? exp)
        (compile-sequence (begin-actions exp) target linkage ct-env)
        )
        ((cond? exp)
        (compile (cond->if exp) target linkage ct-env)
        )
        ((application? exp)
        (compile-application exp target linkage ct-env)
        )
        (else 
            (error "Unknown expression type -- COMPILE" exp)
        )
    )
)

(define (lookup-compile-env var ct-env)
    (define (scan-frame frame)
        (define (iter curr-frame counter)
            (cond ((null? curr-frame)
                #f)
                ((eq? (car curr-frame) var) counter)
                (else (iter (cdr curr-frame (+ 1 counter))))

            )
        )
        (iter frame 0)
    )
    (define (scan curr-env counter)
        (if (null? curr-env)
            (error "Variable not found in env" var)
            (let ((scan-res (scan-frame (car curr-env))))
                (cond 
                    (scan-res (cons counter scan-res))
                    (else (scan (cdr curr-env) (+ counter 1)))
                )
            )
        )

    )
    (scan ct-env 0)
)


(define (compile-variable exp target linkage ct-env)
    (let ((address (lookup-compile-env exp ct-env)))
        (end-with-linkage linkage
            (make-instruction-sequence '(env)
                                        (list target)
                                        `((assign ,target
                                            (op lexical-address-lookup)
                                            (const ,address)
                                            (reg env)
                                        ))
            ) 
        )
    )
)



(define (compile-assignment exp target linkage ct-env)
    (let ((var (assignment-variable exp))
        (get-value-code 
                (compile (assignment-value exp) 'val 'next ct-env)
        )
        )
        (let ((address (lookup-compile-env exp ct-env)))
            (end-with-linkage linkage
                (preserving '(env)
                            get-value-code
                            (make-instruction-sequence 
                                '(env val)
                                (list target)
                                `((perform (op lexical-address-set!)
                                            (const ,address)
                                            (reg val)
                                            (reg env)
                                    )
                                    (assign ,target (const ok))
                                 )
                            )
                )
            )
        )

    )
)

(define (compile-lambda exp target linkage ct-env)
    (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda))
        )
        (let ((lambda-linkage (if (eq? linkage 'next) after-lambda linkage))
              (parameters (lambda-parameters exp))
              (old-ct-env (if (null? ct-env) '() (cons (car ct-env) (cdr ct-env))))
            )
            (set-car! ct-env parameters)
            (set-cdr! ct-env old-ct-env)
            (append-instruction-sequences
                (tack-on-instruction-sequence
                (end-with-linkage lambda-linkage
                    (make-instruction-sequence '(env) (list target)
                    `((assign ,target 
                                (op make-compiled-procedure)
                                (label ,proc-entry)
                                (reg env)
                        )
                    )
                    )
                )
                (compile-lambda-body exp proc-entry ct-env)
                )
                after-lambda
            )
        )
    ) 
)

(define (compile-lambda-body exp proc-entry ct-env)
    (let ((formals (lambda-parameters exp)))
        (append-instruction-sequences
            (make-instruction-sequence
            '(env proc argl)
            '(env)
            `(,proc-entry
                (assign env (op compiled-procedure-env) (reg proc))
                (assign env
                        (op extend-environment)
                        (const ,formals)
                        (reg argl)
                        (reg env)
                )
            )
            ) 
            (compile-sequence (lambda-body exp) 'val 'return ct-env)
        )
    )
)

(define (compile-sequence seq target linkage ct-env)
    (if (last-exp? seq)
        (compile (first-exp seq) target linkage ct-env)
        (preserving 
            '(env continue)
            (compile (first-exp seq) target 'next ct-env)
            (compile-sequence (rest-exps seq) target linkage)
        )
    )
)

(define (compile-application exp target linkage ct-env)
    (let ((proc-code (compile (operator exp) 'proc 'next ct-env))
        (operand-codes
            (map (lambda (operand) (compile operand 'val 'next ct-env))
            (operands exp))
        )
        )
        (preserving '(env continue)
                    proc-code
                    (preserving '(proc continue)
                                (construct-arglist operand-codes)
                                (compile-procedure-call target linkage)
                    )
        )
    )
)

#| Exercice 5.41 |#

(define (find-variable var ct-env)
    (define (scan-frame frame)
        (define (iter curr-frame counter)
            (cond ((null? curr-frame)
                #f)
                ((eq? (car curr-frame) var) counter)
                (else (iter (cdr curr-frame) (+ 1 counter)))

            )
        )
        (iter frame 0)
    )
    (define (scan curr-env counter)
        (if (null? curr-env)
            'not-found
            (let ((scan-res (scan-frame (car curr-env))))
                (cond 
                    (scan-res (cons counter scan-res))
                    (else (scan (cdr curr-env) (+ counter 1)))
                )
            )
        )

    )
    (scan ct-env 0)
)

#| Exercice 5.42 |#


(define (compile-variable exp target linkage ct-env)
    (let ((address (find-variable exp ct-env)))
        (if (eq? address 'not-found)
            (end-with-linkage linkage
                (make-instruction-sequence '(env)
                                        (list target)
                                        `((assign ,target
                                            (op lookup-variable-value)
                                            (const ,exp)
                                            (reg env)
                                            ))
                ) 
            )
            (end-with-linkage linkage
                (make-instruction-sequence '(env)
                                            (list target)
                                            `((assign ,target
                                                (op lexical-address-lookup)
                                                (const ,address)
                                                (reg env)
                                            ))
                ) 
            )
        )

    )
)

(define (compile-assignment exp target linkage ct-env)
    (let ((var (assignment-variable exp))
        (get-value-code 
                (compile (assignment-value exp) 'val 'next ct-env)
        )
        )
        (let ((address (find-variable var ct-env)))
            (if (eq? address 'not-found)
                (end-with-linkage linkage
                    (preserving '(env)
                                get-value-code
                                (make-instruction-sequence 
                                    '(env val)
                                    (list target)
                                    `((perform  (op set-variable-value!)
                                                (const ,var)
                                                (reg val)
                                                (reg env)
                                        )
                                        (assign ,target (const ok))
                                    )
                                )
                    )
                )
                (end-with-linkage linkage
                    (preserving '(env)
                                get-value-code
                                (make-instruction-sequence 
                                    '(env val)
                                    (list target)
                                    `((perform  (op lexical-address-set!)
                                                (const ,address)
                                                (reg val)
                                                (reg env)
                                        )
                                        (assign ,target (const ok))
                                    )
                                )
                    )
                )
            )

        )

    )
)

(define (compile-lambda exp target linkage ct-env)
    (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda))
        )
        (let ((lambda-linkage (if (eq? linkage 'next) after-lambda linkage))
            (parameters (lambda-parameters exp))
            )
            (if (null? ct-env)
                (set! ct-env (list parameters))
                (let ((old-ct-env (cons (car ct-env) (cdr ct-env))))
                    (set-car! ct-env parameters)
                    (set-cdr! ct-env old-ct-env)
                )

            )
            (append-instruction-sequences
                (tack-on-instruction-sequence
                (end-with-linkage lambda-linkage
                    (make-instruction-sequence '(env) (list target)
                    `((assign ,target 
                                (op make-compiled-procedure)
                                (label ,proc-entry)
                                (reg env)
                        )
                    )
                    )
                )
                (compile-lambda-body exp proc-entry ct-env)
                )
                after-lambda
            )
        )
    ) 
)


#| 
Expr to compile 
|#

((lambda (x y)
    (lambda (a b c d)
        ((lambda (y z) (* x y z))
            (* a b x)
            (+ c d x)
        )
    )
) 3 4 )


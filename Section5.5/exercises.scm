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
        '(exp env val proc argl continue unev)
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
            (assign val (reg exp))
            (goto (reg continue))

        ev-variable
            (assign val (op lookup-variable-value) (reg exp) (reg env))
            (goto (reg continue))

        ev-quoted
            (assign val (op text-of-quotation) (reg exp))
            (goto (reg continue))

        ev-lambda 
            (assign unev (op lambda-parameters) (reg exp))
            (assign exp (op lambda-body) (reg exp))
            (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
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
            (assign proc (reg val))
            (test (op no-operands?) (reg unev))
            (branch (label apply-dispatch))
            (save proc)
            (goto (label ev-appl-operand-loop))

        ev-appl-did-operator
            (restore unev)
            (restore env)
            (assign argl (op empty-arglist))
            (assign proc (reg val))
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
            (assign argl (op adjoin-arg) (reg val) (reg argl))
            (assign unev (op rest-operands) (reg unev))
            (goto (label ev-appl-operand-loop))

        ev-appl-last-arg
            (assign continue (label ev-appl-accum-last-arg))
            (goto (label eval-dispatch))

        ev-appl-accum-last-arg
            (restore argl)
            (assign argl (op adjoin-arg) (reg val) (reg argl))
            (restore proc)
            (goto (label apply-dispatch))

        apply-dispatch
            (test (op primitive-procedure?) (reg proc))
            (branch (label primitive-apply))
            (test (op compound-procedure?) (reg proc))
            (branch (label compound-apply))
            (goto (label unknown-procedure-type))

        primitive-apply
            (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
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
            (test (op true?) (reg val))
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
            (perform (op set-variable-value!) (reg unev) (reg val) (reg env))
            (assign val (const ok))
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
            (perform (op define-variable!) (reg unev) (reg val) (reg env))
            (assign val (const ok))
            (goto (reg continue))

        print-result
            (perform (op print-stack-statistics))
            (perform
                (op announce-output)
                (const "::: EC-Eval value:")
            )
            (perform (op user-print) (reg val))
            (goto (label read-eval-print-loop))

        unknown-expression-type
            (assign val (const unknown-expression-type-error))
            (goto (label signal-error))

        unknown-procedure-type
            (restore continue)
            (assign val (const unknown-procedure-type-error))
            (goto (label signal-error))

        signal-error
            (perform (op user-print) (reg val))
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
(assign val (op make-compiled-procedure) (label entry5) (reg env))
(goto (label after-lambda4)) 
entry5 
    (assign env (op compiled-procedure-env) (reg proc)) 
    (assign env (op extend-environment) (const (n)) (reg argl) (reg env)) 
    (save continue) 
    (save env) 
    (assign proc (op lookup-variable-value) (const =) (reg env)) 
    (assign val (const 1)) (assign argl (op list) (reg val)) 
    (assign val (op lookup-variable-value) (const n) (reg env)) 
    (assign argl (op cons) (reg val) (reg argl)) 
    (test (op primitive-procedure?) (reg proc)) 
    (branch (label primitive-branch20)) 

compiled-branch19 
    (assign continue (label after-call18)) 
    (assign val (op compiled-procedure-entry) (reg proc)) 
    (goto (reg val)) 
    
primitive-branch20 
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) 

after-call18 
    (restore env) 
    (restore continue) 
    (test (op false?) (reg val)) 
    (branch (label false-branch7)) 
    
true-branch8 
    (assign val (const 1)) 
    (goto (reg continue)) 
    
false-branch7 
    (assign proc (op lookup-variable-value) (const *) (reg env)) 
    (save continue) 
    (save proc) 
    (assign val (op lookup-variable-value) (const n) (reg env)) 
    (assign argl (op list) (reg val)) 
    (save argl) 
    (assign proc (op lookup-variable-value) (const factorial) (reg env)) 
    (save proc) 
    (assign proc (op lookup-variable-value) (const -) (reg env)) 
    (assign val (const 1)) 
    (assign argl (op list) (reg val)) 
    (assign val (op lookup-variable-value) (const n) (reg env)) 
    (assign argl (op cons) (reg val) (reg argl)) 
    (test (op primitive-procedure?) (reg proc)) 
    (branch (label primitive-branch11)) 
    
compiled-branch10 
    (assign continue (label after-call9)) 
    (assign val (op compiled-procedure-entry) (reg proc)) 
    (goto (reg val)) 
    
primitive-branch11 
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) 
    
after-call9 
    (assign argl (op list) (reg val)) 
    (restore proc) 
    (test (op primitive-procedure?) (reg proc)) 
    (branch (label primitive-branch14)) 
    
compiled-branch13 
    (assign continue (label after-call12)) 
    (assign val (op compiled-procedure-entry) (reg proc)) 
    (goto (reg val)) 
    
primitive-branch14 
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))

after-call12 
    (restore argl) 
    (assign argl (op cons) (reg val) (reg argl)) 
    (restore proc) 
    (restore continue) 
    (test (op primitive-procedure?) (reg proc)) 
    (branch (label primitive-branch17)) 
    
compiled-branch16 
    (assign val (op compiled-procedure-entry) (reg proc)) 
    (goto (reg val)) 
    
primitive-branch17 (
    assign val (op apply-primitive-procedure) (reg proc) (reg argl)) 
    (goto (reg continue)) 
    
after-call15 

after-if6 

after-lambda4 
    (perform (op define-variable!) (const factorial) (reg val) (reg env)) 
    (assign val (const ok)))

|#
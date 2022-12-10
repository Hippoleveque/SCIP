#| Exercise 5.23 |#

#| 

eval-dispatch
    (test (op self-evaluating?) (reg exp))
    (branch (label ev-self-eval))
    (test (op variable?) (reg exp))
    (branch (label ev-variable))
    (test (op quoted?) (reg exp))
    (branch (label (ev-quoted)))
    (test (op assignment?) (reg exp))
    (branch (label (ev-assigment)))
    (test (op definition?) (reg exp))
    (branch (label (ev-definition)))
    (test (op if?) (reg exp))    
    (branch (label (ev-if)))
    (test (op lambda?) (reg exp))
    (branch (label ev-lambda))
    (test (op begin?) (reg exp))
    (branch (label ev-begin))
    (test (op cond?) (reg exp))
    (branch (label ev-cond))
    (test (op let?) (reg exp))
    (branch (label ev-let))
    (test (op application?) (reg exp))
    (branch (label ev-application))
    (goto (label unknown-expression-type))

ev-cond 
    (assign exp (op cond->if) (reg exp))
    (goto (label eval-dispatch))

ev-let
    (assign exp (op let->combination) (reg exp))
    (goto (label eval-dispatch))
|#

#| Exercise 5.24 |#

#| 
ev-cond
    (save continue)
    (save env)
    (goto (label ev-loop))

ev-loop
    (test (op last-clause?) (reg exp))
    (goto (label ev-cond-last-clause))
    (save exp)
    (assign exp (op first-clause) exp)
    (assign unev (op cond-consequent) (reg exp))
    (assign exp (op cond-predicate) (reg exp))
    (save unev)
    (assign continue (label cond-decide))
    (goto (label eval-dispatch))

cond-decide
    (restore unev)
    (test (op true?) (reg val))
    (goto (label ev-sequence))
    (restore exp)
    (assign exp (op rest-clauses) (reg exp))

ev-cond-last-clause
    (assign exp (op first-clause) exp)
    (assign unev (op cond-consequent) (reg exp))
    (goto (label ev-sequence))

|#

#| Exercise 5.25 |#


#| 
ev-application
    (save continue)
    (save env)
    (assign unev (op operands) (reg exp))
    (save unev)
    (assign exp (op operator) (reg exp))
    (assign continue (label ev-appl-did-operator))
    (goto (label eval-dispatch))

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
    (assign exp (op delay-it) (reg exp) (reg env))
    (test (op last-operand?) (reg unev))
    (branch (label (label ev-appl-last-arg)))
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
    (goto (label unknown procedure-type))

primitive-apply
    (assign unev (op empty-arglist))

primitive-apply-loop
    (test (op empty-arglist?) (reg argl))
    (branch (label primitive-eff-apply))
    (assign exp (op car) (reg argl))
    (assign exp (op actual-value) (reg exp))
    (assign unev (op adjoin) (reg unev) (reg exp))
    (assign argl (op cdr) (reg argl))
    (goto (label primitive-apply-loop))

primitive-eff-apply
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    (restore continue)
    (goto (reg continue))

compound-apply
    (assign unev (op procedure-parameter) (reg proc))
    (assign env (op procedure-environment) (reg proc))
    (assign env (op extend-environment) (reg env))
    (assign unev (op procedure-body) (reg proc))
    (goto (label ev-sequence))

|#

#| Exercise 5.26 |#

(define (factorial n)
    (define (iter product counter)
        (if (> counter n)
            product
            (iter (* counter product)
                  (+ counter 1)
            )
        )
    )
    (iter 1 1)
)

#| 

a.

(factorial 1) => 64 pushes, 10 depth
(factorial 2) => 99 pushes, 10 depth
(factorial 3) => 134 pushes, 10 depth
(factorial 4) => 169 pushes, 10 depth
(factorial 5) => 204 pushed, 10 depth
...

This depth is ten and does not change (because 
we are indeed tail-recursive).
It correspond to the recursive evaluation of iter. 

b. 

64 + 35 * (n - 1)

|#

#| c. Trying with non tail-recursive eval |#

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
            (save env)
            (assign unev (op operands) (reg exp))
            (save unev)
            (assign exp (op operator) (reg exp))
            (assign continue (label ev-appl-did-operator))
            (goto (label eval-dispatch))

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
            (test (op no-more-exps?) (reg unev))
            (branch (label ev-sequence-end))
            (assign exp (op first-exp) (reg unev))
            (save unev)
            (save env)
            (assign continue (label ev-sequence-continue))
            (goto (label eval-dispatch))

        ev-sequence-continue
            (restore env)
            (restore unev)
            (assign unev (op rest-exps) (reg unev))
            (goto (label ev-sequence))

        ev-sequence-end
            (restore continue)
            (goto (reg continue))

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

(define (no-more-exps? seq) (null? seq))

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
        (list 'no-more-exps? no-more-exps?)
        (list '= =)
        (list '* *)
        (list '+ +)
    )
)

#| 
(factorial 1) => 70 pushes, 17 depth
(factorial 2) => 107 pushes, 20 depth
(factorial 3) => 144 pushes, 23 depth
(factorial 4) => 181 pushes, 26 depth
(factorial 5) => 218 pushed, 29 depth
...

Here we see that the depth is also linear in n.

|#

#| Exercise 5.27 |#

(define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n)
    )
)


#| 
                    | Max depth         | Number of pushes  |
-------------------------------------------------------------
Recursive factorial |   8 + 5 * (n - 1) | 16 + 32 * (n - 1) |
-------------------------------------------------------------
Iterative factorial |       10          | 64 + 35 * (n - 1) |
------------------------------------------------------------
|#

#| Exercise 5.28 |#

#| Already done in 5.26 as added exercise |#

#| Exercice 5.29 |#

(define (fib n)
    (if (< n 2)
        n
        (+ (fib (- n 1)) (fib (- n 2)))
    )
)

#| a. |#

#| 
Following experiments : 8 + 5 * (n - 1)
|#

#| b. |#

#| 
16 + 72 = 88 + 40 = 128 
72 + 128 + 40 = 240

S(n) = S(n - 1) + S(n - 2) + 40

Fib(n) = Fib(n - 1) + Fib(n - 2)

Let's assume S(n) = a * Fib(n + 1) + b

S(n - 1) + S(n - 2) + 40 = a * Fib(n + 1) + b

a * Fib(n) + a * Fib(n - 1) + 2b + 40 = a * Fib(n + 1)
Fib(n + 1) = Fib(n) + Fib(n - 1)
a * Fib(n) + a * Fib(n - 1) + b + 40 = a * Fib(n) + a * Fib(n -1)
b + 40 = 0 => b = - 40

S(3) = 128  = a * Fib(4) - 40
            = a * 3 - 40 
        a = (128 + 40) / 3
        a = 168 / 3 
        a = 56

56 * 55 - 40 = 3040 OK what was expected

S(n) = 56 * Fib(n + 1) - 40

|#

#| Exercice 5.30 |#

#| Won't do |#



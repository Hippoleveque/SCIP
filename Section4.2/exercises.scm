#| Exercise 4.25 |#

(define (factorial n)
    (unless (= n 1)
            (factorial (- n 1))
            1
    )
)   

#| 
In normal order, 
we enter (factorial 1), (factorial 0)
is not evaluated and we return 1.

In applicative-order,
we enter (factorial 1),
(factorial 0 needs to be evaluated)
and we re-enter (factorial -1)
then (factorial -2) infinitely

|#

#| Exercise 4.26 |#

#| a. As a derived procedure that takes delayed procedures |#



(define (unless? exp)
    (tagged-list? exp 'unless)
)

(define (unless-condition exp)
    (car exp)
)

(define (unless-usual-proc exp)
    (cadr exp)
)

(define (unless-exceptional-proc exp)
    (caddr exp)
)


(define (eval-useless exp env)
    (make-if (unless-condition exp)
             (unless-exceptional-proc exp)
             (unless-usual-proc exp)
    )
)


#| 

To use unless with higher order procedures we need to 
take procedure as arguments

|#


#| Exercise 4.27 |#

(define count 0)

(define (id x)
    (set! count (+ count 1))
    x
)

#| 

;;; L-Eval input:
==> (define w (id (id 10)))

;;; L-Eval input:
==> count

1. Eval define ==> enter the eval loop,
2. enter the eval-definition
3. Enter the eval of (id (id 10))
4. We force the evaluation of the operator 
5. We enter the eval-sequence with the new
environment that contains the delayed arg (id 10)
6. We enter the eval of (set! count (+ count 1))
7. We enter the eval-assignemnt 
8. We set the count to count + 1 ==> count = 1
9. We enter the eval of the second expression in the sequence
10. It is a variable bind to a delayed arg (id 10)
11. So it is a thunk with (id 10)

;;; L-Eval value:
--> 1

;;; L-Eval input:
==> w

We force the evaluation of w (from driver-loop)
so the thunk id 10 is forced to evaluate,
it becomes 10 and count becomes 2

;;; L-Eval value:
--> 10

;;; L-Eval input:
==> count

;;; L-Eval value:
==> 2

|#


#| Exercise 4.28 |#

#| If we have a lambda expression as the operator for 
If we do not evaluate it would still be a thunk
and after that the rest of the code will not work
because it has not any procedure body or whatever

http://community.schemewiki.org/?sicp-ex-4.28 is greater explanation

|#

#| Exercise 4.29 |#

(define (square x)
    (* x x)
)

#| 
;;; L-Eval input:
==> (square (id 10))

1. We enter evaluation of an application
2. Square is force-evaluated
3. It is a compound procedure so we enter
the eval-sequence
4. We enter the first expression of eval-sequence 
(* x x)
5. The env bindings x is the thunk (id 10)
6. We enter the evaluation of * which is 
a primitive-procedure 
7. We force evaluate x and x again 
8. Count is increased to 2 
9 Result is 100

;;; L-Eval value:
==> 100 (both case)

;;; L-Eval input:
==> count

;;; L-Eval value:
a. Memoized 
==> 1
Because the object x is first evaluated, then
it it transformed by the force-it procedure and
becomes an evaluated-thunk, and when evaluated again
it is already 10 so we do not rerun the code

b. Not Memoized
==> 2
The object x is evaluated twice so the code 
runs twice and therefore count is increased twice
to 2

|#

#| Exercise 4.30 |#

#| a.  |#

(define (for-each proc items)
    (if (null? items)
        'done
        (begin (proc (car items))
               (for-each proc (cdr items))
        )
    )
)

;;; L-Eval input:
(for-each (lambda (x) (newline) (display x)) (list 57 321 88))

#| 
1. We enter the evaluation and we detect this is an application
2. It is a compound procedure, so we force-eval the operator
3. We enter evaluation of the first exp in the sequence of for-each,
The lambda and the list are thunked
4. We enter the eval-if, we force-evaluate the (null? items)
5. As null? is a primitive procedure, the items thunk is 
force-evaluated
6. We enter evaluation of the begin
7. We enter eval-sequence
8. The first exp in the sequence enters evaluation
9. The operator (lambda) is force-evaluted
10. This is a compound procedure
11. We enter eval-sequence of the lambda,
(car items) is thunked
12. We evaluate newline who gots evaluated,
a newline is printed
13. We evaluate (display x)
14. This is a primitive procedure so x gets
force-evaluated, and as car is also a primitive procedure,
10 is displayed
15. The second exp in the sequence of the begin gets evaluated
16. We reenter the cycle, except the thunked have already been
evaluated

So we get all the side effects we want

|#

#| b. |#

(define (p1 x)
    (set! x (cons x '(2)))
    x
)

(define (p2 x)
    (define (p e)
        e
        x
    )
    (p (set! x (cons x '(2))))
)

(p1 1)

(p2 1)

#| 


I. With the original eval-sequence

1. We enter eval-assignment for p1
2. We create the lambda and associate it with p1
3. We enter eval-assignment for p2
4. We create the lambda and associate it with p2

5. We evaluate (p1 1)
6. Is is an application so we force-evaluate
p1 (ez it is a variable)
7. We enter the apply
8. We thunk 1 (to x)
9. We enter eval-sequence
10. We enter the evaluaton of 
(set! x (cons x '(2)))
11. We enter eval assignment
12. We eval (cons x '(2)) which is force-evaluated
13. x is force-evaluated to 1
14. x is associated with cons (1 '(2)
so the value of p1 1 is (1 '(2))

14. We evaluate (p2 1)
15. Is is an application so we force-evaluate
p2 (ez it is a variable)
16. We enter the apply
17. We thunk 1 to x
18. We enter eval-sequence
19. We enter the evaluation of the define 
20. In the current frame, p is defined
21. We enter the evaluation of (p (set! x (cons x '(2))))
22. This is an application, so p is force-evalutated 
(ez cuz variable)
23. We enter the apply
24. (set! x (cons x '(2))) is thunked
25. We enter-eval sequence
26. We enter the first exp of p 
27. e is evaluated, it is a thunk so nothing happen because 
it is not force-evaluated
26. x is force-evaluated so the 1 thunk is evaluated and x is 1

With Cy's, we would force evaluate the thunk and therefore set x
correctly like in p1

|#

#| c. |#

#| Because we are only 
using primtive procedure in lambda


|#

#| d. |#

#| If Cy's hypothesis is true, then
I think his way is better.
|#

#| Exercise 4.31 |#

#| 1. Changes to make in eval |#

(define (eval-definition exp env)
    (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env) 
                    env
    )
    'ok
)

#| Changes to make to apply |#

((compound-procedure? procedure)
    (eval-sequence (procedure-body procedure)
            (extend-environment
                    (procedure-parameters procedure)
                    (list-of-processed-args (procedure-parameters-type procedure)
                                            arguments 
                                            env
                    )
                    (procedure-environment procedure)
            )
    )
)

(define (procedure-parameters p)
    (map (lambda (x) (if (pair? x) (car x) x)) (cadr p))
)

(define (procedure-parameters-type p)
    (map (lambda (x) (if (pair? x) (cdr x) 'normal)) (cadr p))
)

(define (list-of-processed-args types exps env)
    (if (no-operands? exps)
        '()
        (cons 
            (cond ((no-operands? exps) '())
                ((eq? 'normal (car types)) (actual-value (first-operand exps) env))
                ((eq? 'lazy (car types)) (delay-it-lazy (first-operand exps) env))
                ((eq? 'lazy-memo (car types)) (delay-it-lazy-memo (first-operand exps) env))
            )
            (list-of-processed-args (cdr types) (rest-operands exps) env)
        )
    )   
)

(define (delay-it-lazy exp env)
    (list 'thunk exp env)
)

(define (delay-it-lazy-memo exp env)
    (list 'thunk-memo exp env)
)

(define (thunk? obj)
    (tagged-list? obj 'thunk)
)

(define (thunk-memo? obj)
    (tagged-list? obj 'thunk-memo)
)


(define (force-it obj)
    (cond ((thunk? obj)
           (actual-value (thunk-exp obj)
                         (thunk-env obj)
           )
          )
          ((thunk-memo? obj)
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

#| Exercise 4.32 |#

#| http://community.schemewiki.org/?sicp-ex-4.32 |#

#| Exercise 4.33 |#

(define (text-of-quotation expr)
    (if (pair? (cadr expr))
        (make-list (cadr expr))
        (cadr expr)
    )
)

(define (make-list expr)
    (if (null? expr)
        (list 'quote ())
        (list 'cons (list 'quote (car expr)) (make-list (cdr expr)))
    )
)

#| Exercise 4.34 |#

#| Wont do |#

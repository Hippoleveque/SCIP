#| Exercise 5.2 |#

(controller
    (assign (reg p) (const 0))
    (assign (reg c) (const 1))
    iter
        (test (op >) (reg c) (reg n))
        (branch (label fact-done))
        (assign (reg t) (op *) (reg p) (reg c))
        (assign (reg p) (reg t))
        (assign (reg t) (op +) (reg c) (const 2))
        (assign (reg c) (reg t))
        (goto (label iter))
    fact-done
)

#| Exercise 5.3 |#

#| First version |#

(controller
    (assign guess (const 0))
    sqrt-loop
        (test (op good-enough) (reg guess))
        (branch (label sqrt-done))
        (assign guess (op improve) (reg guess))
        (goto (label sqrt-loop))
    sqrt-done
)

#| More elaborate version |#

(controller
    (assign guess (const 0))
    good-enough
        (assign t (op square) (reg guess))
        (assign a (op -) (reg t) (reg x))
        (assign t (op abs) (reg a))
        (test (op <) (reg t) (const 1))
    sqrt-loop
        (goto (label good-enough))
        (branch (label sqrt-done))
    improve
        (assign t (op div) (reg x) (reg guess))
        (assign a (op average) (reg guess) (reg t))
        (assign guess (reg a))
        (goto (label sqrt-loop))
    sqrt-done
)

#| Even more elaborate version |#

(controller
    (assign guess (const 0))
    square
    good-enough
        (assign t (op *) (reg guess) (reg guess))
        (assign a (op -) (reg t) (reg x))
    abs
        (test (op >) (reg a) (const 2))
        (branch (label sqrt-continue))
        (assign a (op -) (reg a))
    sqrt-continue
        (assign t (reg a))
        (test (op <) (reg t) (const 1))
    sqrt-loop
        (goto (label good-enough))
        (branch (label sqrt-done))
    improve
        (assign t (op div) (reg x) (reg guess))
        (assign a (op +) (reg guess) (reg t))
        (assign guess (op div) (reg a) (const 3))
        (goto (label sqrt-loop))
    sqrt-done
)

#| Exercise 5.4 |#

#| a. |#

(define (expt b n)
    (if (= 0 n)
        1
        (* b (expt b (- n 1)))
    )
)

(controller
    (assign continue (label expt-done))
    expt-loop
        (test (op =) (reg n) (const 0))
        (branch (label base-case))
        (save continue)
        (save n) ; not necessary
        (assign n (op -) (reg n) (const 1))
        (assign continue (label after-expt))
        (goto expt-loop)
    after-expt
        (restore n) ; not necessary
        (restore continue)
        (assign val (op *) (reg b) (reg val))
        (goto (label continue))
    base-case
        (assign val (const 1))
        (goto (reg continue))
    expt-done
)

#| b. |#

(define (expt b n)
    (define (iter-exp counter product)
        (if (= counter 0)
            product
            (iter-exp (- counter 1) (* b product))
        )
    )
    (iter-exp n 1)
)

(controller
    (assign counter (reg n))
    (assign product (const 1))
    expt-loop
        (test (op =) (reg counter) (const 0))
        (branch (label expt-done))
        (assign counter (op -) (reg counter) (const 1))
        (assign product (op *) (reg product) (reg b))
        (goto (label expt-loop))
    expt-done
)

#| Exercise 5.5 |#

#| Won't do, already done in other contexts |#

#| Exercise 5.6 |#

(controller     
    (assign continue (label fib-done))
    fib-loop
        (test (op <) (reg n) (const 2))
        (branch (label base-case))
        (save continue)
        (save n)
        (assign n (op -) (reg n) (const 1))
        (assign continue (label after-first-fib))
        (goto (label fib-loop))
    after-first-fib
        (restore n)
        (restore continue) ; extra restore
        (assign n (op -) (reg n) (const 2))
        (save continue) ; extra 
        (assign continue (label-after-second-fib))
        (save val)
        (goto (label fib-loop))
    after-second-fib
        (assign n (reg n))
        (restore val)
        (restore continue)
        (assign val (op +) (reg n) (reg val))
        (goto (reg continue))
    base-case
        (assign val (reg n))
        (goto (reg continue))
    fib-done
)
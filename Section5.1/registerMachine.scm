(data-paths
    (registers
        ((name a) (buttons ((name a<-b) (source (register b)))))
        ((name b) (buttons ((name b<-t) (source (register t)))))
        ((name t) (buttons ((name t<-r) (source (operation rem)))))
    )
)

(operations
    ((name rem) (inputs (register a) (register b)))
    ((name =) (inputs (register b) (constant 0)))
)

(controller
    test-b
        (test =)
        (branch (label gcd-done))
        (t<-r)
        (a<-b)
        (b<-t)
        (goto (label test-b))
    gcd-done
)

#| Simpler |#

(controller
    test-b
        (test (op =) (reg b) (const 0))
        (branch (label gcd-done))
        (assign t (op rem) (reg a) (reg b))
        (assign a (reg b))
        (assign b (reg t))
        (goto (label test-b))
    gcd-done
)

#| Gcd machine that reads inputs |#

(controller 
    gcd-loop
        (assign a (op read))
        (assign b (op read))
    test-b
        test-b
        (test (op =) b (const 0))
        (branch (label gcd-done))
        (assign t (op rem) (reg a) (reg b))
        (assign a (reg b))
        (assign a (reg t))
        (goto (label test-b))
    gcd-done
        (perform (op print) (reg a))
        (goto (label gcd-loop))
        
)

#| More elaborate, with remainder replaced |#

(controller 
    gcd-loop
        (assign a (op read))
        (assign b (op read))
    test-b
        test-b
        (test (op =) (reg b) (const 0))
        (branch (label gcd-done))
        (assign t (reg a))
    rem-loop
        (test (op <) (reg t) (reg b))
        (branch (label rem-done))
        (assign t (op -) (reg t) (reg b))
        (goto (rem-loop))
    rem-done
        (assign a (reg b))
        (assign b (reg t))
        (goto (label test-b))
    gcd-done
        (perform (op print) (reg a))
        (goto (label gcd-loop))
)

#| Factorial with a register machine 
with stack implemented. 
|#

(controller 
    (assign continue (label fact-done))
    fact-loop
        (test (op =) (reg n) (const 1))
        (branch (label base-case))
        (save continue)
        (save n)
        (assign n (op -) (reg n) (const 1))
        (assign continue (label after-fact))
        (goto (label fact-loop))
    after-fact
        (restore n)
        (restore continue)
        (assign val (op *) (reg val) (reg n))
        (goto (reg continue))
    base-case
        (assign val (const 1))
        (goto (reg continue))
    fact-done
)

#| (Naive) Fibonacci with a register machine 
with stack implemented. 
|#

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
        (restore continue)
        (assign n (op -) (reg n) (const 2))
        (save continue)
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
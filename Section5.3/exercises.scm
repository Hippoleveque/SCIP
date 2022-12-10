#| Exercise 5.20 |#

#| 

(define x (cons 1 2))

p1 represents x, free's value is p2

free is updated to p2, then to p3 to allow for the creation of y

At the end free's value is p4

|#

#| Exercise 5.21 |#

#| a. |#

(define (count-leaves tree)
    (cond ((null? tree) 0)
          ((not (pair? tree) 1))
          (else (+ (count-leaves (car tree)) 
                   (count-leaves (cdr tree))
                )
          )
    )
)

#| 
(read-loop
    (assign tree (op read))
    (assign continue (label count-leaves-done))
count-leaves-loop
    (test (op eq?) (reg tree) (const e0))
    (branch (label base-case-1))
    (test (op pair?) (reg tree))
    (branch (label left-tree))
    (save continue)
    (save tree)
    (assign tree (op car) (reg tree))
    (assign continue (label after-count-leaves-1)))
    (goto (label count-leaves-loop))
left-tree
    (assign val (const 1))
    (goto (reg continue))
after-count-leave-1
    (restore tree)
    (save val)
    (assign tree (op cdr) (reg tree))
    (assign continue (label-after-count-leave2))
    (goto (label count-leaves-loop))
after-count-leave-2
    (assign tree (reg val))
    (restore val)
    (restore continue)
    (assign val (op +) (reg val) (reg tree))
    (goto (reg continue))
base-case-1
    (assign val (const 0))
    (goto (reg continue))
base-case-1
    (assign val (const 1))
    (goto (reg continue))
count-leaves-done
    (perform (op print) (reg val))
    (goto (label read-loop))
)

|#

#| b. |#


(define (count-leaves tree)
    (define (count-iter tree n)
        (cond ((null? tree) n)
              ((not (pair? tree)) (+ 1 n))
              (else (count-iter (cdr tree)
                                (count-iter (car tree) n)
                    )
              )
        )
    )
)

#| 
(read-loop
    (assign tree (op read))
    (assign n (const 0))
    (assign continue (label count-leaves-done))
count-leaves-loop
    (test (op eq?) (reg tree) (const e0))
    (goto (reg continue))
    (test (op not-pair?) (reg tree))
    (goto (label after-pair))
    (save continue)
    (save tree)
    (assign tree (op car) (reg tree))
    (assign continue (label after-count-leaves)))
    (goto (label count-leaves-loop))

after-count-leaves
    (restore tree)
    (restore continue)
    (assign tree (op cdr) (reg tree))
    (goto (label count-leaves-loop))

after-pair 
    (assign n (op +) (const 1) (reg n))
    (goto (reg continue))

count-leaves-done
    (perform (op print) (reg n))
    (goto (label read-loop))
)

|#


#| To be finished after implmementation |#

#| Exercise 5.22 |#

(define (append x y)
    (if (null? x)
        y
        (cons (car x) (append x y))
    )
)

#| 


(
start
    (assign continue (label base-case))
    
append-loop
    (test (op eq?) (reg x) (const e0))
    (goto (label base-case))
    (save car-val)
    (save continue)
    (assign car-val (op car) (reg x))
    (assign continue (label after-append))
    (goto (label append-loop))

after-append 
    (restore continue)
    (restore car-val)
    (assign res (op cons) (reg car-val) (reg res))
    (goto (reg continue))

base-case
    (assign res (reg y))
    (goto (reg continue))

append-done

)

|#

(define (append x y)
    (set-cdr! (last-pair x) y)
)
#|

(
start
    (assign continue (label base-case))
    (assign tmp (reg x))
append-loop
    (test (op eq?) (reg x) (const e0))
    (branch (label base-case))
    (assign tmp (op cdr) (reg tmp))
    (goto (label append-loop))


base-case
    (perform (op vector-set!) (reg the-cdrs) (reg tmp) (reg y))
)

|#
#| Exercice 3.12 |#

(define (append! x y)
    (set-cdr! (last-pair x) y)
    x
)

(define (last-pair x)
    (if (null? (cdr x))
        x
        (last-pair (cdr x))
    )
)

(define x (list 'a 'b))

(define y (list 'c 'd))

(define z (append x y))

#| (cdr x) -> (b) ; x has not been mutated  |#

(define w (append! x y))

#| (cdr x) -> (b c d) ; x has been mutated by append! |#


#| Exercice 3.13 |#

(define (make-cycle x)
    (set-cdr! (last-pair x) x)
    x
)

#| 
(define z (make-cycle (list 'a 'b 'c)))

Done in notebook, basically the last pair of z points to the first one 

If we try to compute (last-pair z), we will enter an infinite loop

(Displaying z on the interpreter print something weird)

|#

#| Exercice 3.14 |#

(define (mystery x)
    (define (loop x y)
        (if (null? x)
            y
            (let ((temp (cdr x)))
                (set-cdr! x y)
                (loop temp x)
            )
        )
    )
    (loop x '())
)

#| 
In general mystery reverse the list x

(define v (list 'a 'b 'c 'd))
(define w (mystery v))

==> v -> (a) 
==> w -> (d c b a)
|#

#| Exercice 3.15 |#


(define (set-to-wow! x)
    (set-car! (car x) 'wow)
    x
)

(define x (list a b))
(define z1 (cons x x))
(define z2 (const (list 'a 'b) (list 'a 'b)))

#| 
(set-to-wow! z1) -> Modifies the car of x from 'a to 'wow,
So the list becomes ((wow b) wow b)

(set-to-vow! z2) -> Modifies the car of the car of z2, 
from 'a to vow but the cdr of z2 is left untouched,
So the list becomes ((wow b) a b)
|#

#| Exercice 3.16 |#

#| Done on a notebook, the idea is that we can re-reference an already existing pair that will be counted twice
Picture of the exercice in notebook, the only thing missing is the infinite case,
Infinite case is obtained by self-referencing (or referencing any pair that comes first)
in the first order
|#

#| Exercice 3.17 |#

(define empty-set '())
(define (element-of-set? el s)
    (cond ((null? s) #f)
          ((eq? (car s) el) #t)
          (else (element-of-set? el (cdr s)))

    )
)

(define (add-element el s)
    (cons el s)
)


(define (count-pairs x)
    (let ((counted '()))
        (define (iter rest)
            (cond ((not (pair? rest)) 0) 
                  ((element-of-set? rest counted) 0)
                  (else (begin (set! counted (cons rest counted)) (+ 1 (iter (car rest)) 
                                                                       (iter (cdr rest)))))
                
            )
        )
        (iter x)
    )
)

(count-pairs (list 'a 'b 'c))

(define z (list 'c))
(define y (cons 'b z))
(define x (cons z y))

(count-pairs x)

(define z (list 'c))
(define y (cons z z))
(define x (cons y y))

(count-pairs x)

#| Exercice 3.18 |#

(define (has-infinite-loop? x)
    (let ((seen '()))
        (define (iter y)
            (cond ((null? y) #f)
                  ((element-of-set? y seen) #t)
                  (else (begin (set! seen (cons y seen))
                    (iter (cdr y))
                  ))
            )    
        )
        (iter x)
    )
)

#| Exercice 3.19 |#

#| Based on Floyd's idea of the toitoise and the hare, from http://community.schemewiki.org/?sicp-ex-3.19 |#

(define (contains-cycle? lst) 
    (define (safe-cdr l) 
    (if (pair? l) 
        (cdr l) 
        '())) 
    (define (iter a b) 
    (cond ((not (pair? a)) #f) 
            ((not (pair? b)) #f) 
            ((eq? a b) #t) 
            ((eq? a (safe-cdr b)) #t) 
            (else (iter (safe-cdr a) (safe-cdr (safe-cdr b)))))) 
    (iter (safe-cdr lst) (safe-cdr (safe-cdr lst)))) 



#| Exercice 3.20 |#

#| Done on notebook, see http://community.schemewiki.org/?sicp-ex-3.20 |#


#| Exercice 3.21 |#

#| 
To explain the example, when we do delete queue with
one remaining element, we don't change the rear pointer
so that it still point to the deleted element. 
It does not matter because the selectors are still working,
empty-queue? returns true etc and if we add after that another
element within the q rear-ptr will point to it as it is supposed to.

Print q is as follow 
|#

(define (print-queue q) (car q))

#| Exercice 3.22 |#

(define (make-queue)
    (let ((front-ptr '())
         (rear-ptr '()))
         (define (set-front-ptr! v) (set! front-ptr v))
         (define (set-rear-ptr! v) (set! rear-ptr v))
         (define (empty-queue?) (null? front-ptr))
         (define (front-queue) 
            (if (empty-queue?)
                (error "The queue is empty" q)
                (car front-ptr)
            )
         )
         (define (insert-queue! item)
            (let ((new (cons item '())))
                (if (empty-queue?)
                    (begin (set-front-ptr! new) (set-rear-ptr! new))
                    (begin (set-cdr! rear-ptr new) (set-rear-ptr! new) (print-queue))
                )
            )
         )
         (define (delete-queue!)
            (if (empty-queue?)
                (error "The queue is empty" q)
                (begin (set-front-ptr! (cdr front-ptr)) (print-queue))
            )
         )
         (define (print-queue)
            front-ptr
         )


         (define (dispatch m)
            (cond ((eq? m 'empty-queue?) empty-queue?)
                  ((eq? m 'front-queue) front-queue)
                  ((eq? m 'insert-queue!) insert-queue!)
                  ((eq? m 'delete-queue!) delete-queue!)
                  ((eq? m 'print-queue) print-queue)
                  (else (error "Operation not supported" m))
        
            )
         )
        dispatch
    )
)


#| Exercice 3.23 |#

(define (make-deque)
    (cons '() '())
)

(define (front-ptr q) (car q))
(define (rear-ptr q) (cdr q))
(define (empty-deque?) (null? front-ptr q))
(define (set-front-ptr! q item) (set-car! q item))
(define (set-rear-ptr! q item) (set-cdr! q item))

(define (front-deque q)
    (if (empty-deque? q)
        (error "The queue is empty" q)
        (caar (front-ptr q))
    )
)

(define (rear-deque q)
    (if (empty-deque? q)
        (error "The queue is empty" q)
        (caar (rear-ptr q))
    )
)

(define (front-insert-deque! q item)
    (let ((new (cons (cons item '()) (front-ptr q))))
        (if (empty-queue? q)
            (begin (set-front-ptr! q new) (set-rear-ptr! q new))
            (begin (set-cdr! (car (front-ptr q)) new) (set-front-ptr! q new))
        )
    )
)

(define (rear-insert-deque! q item)
    (let ((new (cons (cons item (rear-ptr q)) '())))
        (if (empty-queue? q)
            (begin (set-front-ptr! q new) (set-rear-ptr! q new))
            (begin (set-cdr! (rear-ptr q) new) (set-rear-ptr! q new))
        )
    )
)

(define (front-delete-deque! q)
    (cond ((empty-queue? q) (error "The queue is empty" q))
          ((eq? (front-ptr q) (rear-ptr q)) 
           (set-front-ptr! q '())
           (set-rear-ptr! q '())
          )
          (else (set-front-ptr! q (cdr (front-ptr q))))
    )
)

(define (rear-delete-deque! q)
    (cond ((empty-queue? q) (error "The queue is empty" q))
        ((eq? (front-ptr q) (rear-ptr q)) 
         (set-front-ptr! q '())
         (set-rear-ptr! q '())
        )
        (else (set-rear-ptr! q (cdr (car (rear-ptr q)))))
    )
)

(define (print-deque q)
    (define (helper qlist)
        (if (null? qlist)
            '()
            (cons (caar qlist) (helper (cdr qlist)))
        )
    )
    (helper (front-ptr q))
)


#| Exercice 3.24 |#

#| We will make the exercise with 1-D table |#


(define (make-table same-key?)

    (define (assoc key records)
        (cond ((null? records) #f)
              ((same-key? (caar records) key) (car records))
              (else (assoc key (cdr records)))
        )
    )
    (let ((table (cons '*table* '())))


        (define (lookup key)
            (let ((record (assoc key (cdr table))))
                    (if record
                        (cdr record)
                        #f
                    )
            )           
        )

        (define (insert! key value)
            (let ((record (assoc key (cdr table))))
                (if record
                    (set-cdr! record value)
                    (set-cdr! table (cons (cons key value) (cdr table)))
                )
            )
        )

        (define (dispatch m)
            (cond ((eq? m 'lookup) lookup)
                  ((eq? m 'insert!) insert!)
                  (else (error "Operation not supported" m))
            )
        )
        dispatch
    )
)

(define (same-key? x y)
     (< (abs (- x y)) 5)
)

#| Exercice 3.25 |#

(define (make-table)
    (define (assoc key records)
        (cond ((null? records) #f)
              ((equal? (caar records) key) (car records))
              (else (assoc key (cdr records)))
        )
    )
    (let ((table (cons '*table* '())))
        (define (lookup keys)
            (define (iter rest-keys curr-table)
                (cond ((null? rest-keys) (cdr curr-table))
                      (else (
                        let ((subtable (assoc (car rest-keys) (cdr curr-table))))
                            (if subtable
                                (iter (cdr rest-keys) subtable)
                                #f
                            )
                      ))
                )
            )
            (iter keys table)
        )

        (define (insert! keys value)
            (define (helper rest-keys end)
                (cond ((null? rest-keys) value)
                    (else (cons (cons (car rest-keys) (helper (cdr rest-keys) '())) end))
                )
            )  

            (define (iter rest-keys curr-table)
                (cond ((null? rest-keys) (set-cdr! curr-table value))
                    (else (
                            let ((subtable (assoc (car rest-keys) (cdr curr-table))))
                                (if subtable
                                    (if (pair? (cdr subtable))
                                        (iter (cdr rest-keys) subtable)
                                        (set-cdr! subtable (helper (cdr rest-keys) '()))
                                    )
                                    (set-cdr! curr-table (helper rest-keys (cdr curr-table)))
                                )
                    ))
                )
            )
            (iter keys table)
        )
            
        (define (dispatch m)
            (cond ((eq? m 'lookup) lookup)
                    ((eq? m 'insert!) insert!)
                    (else (error "Operation not supported" m))
            )
        
        )
        dispatch
    )
)


#| Exercice 3.26 |#

#| 
A more efficient way to implement the table would be to 
make a binary tree where nodes are a pair (key . value), 
the comparison methods would be applied on the key. 
Like in the current examples the value could be itself another tree.
|#


#| Exercice 3.27 |#

(define (memoize f)
    (let ((table (make-table)))
        (lambda (x)
            (let ((previously-computed-result ((table 'lookup) (list x))))
                (or previously-computed-result (
                    let ((y (f x)))
                        ((table 'insert!) (list x) y)
                        y
                ))
            )
        )
    )
)

(define memo-fib
    (memoize (lambda (n) (
        cond ((= n 0) 0)
             ((= n 1) 1)
             (else (+ (memo-fib (- n 1)) (memo-fib (- n 2))))
    )))
)

#| 

Won't do the drawing, the idea is that memo-fib points to 
a pair (code E1) where E1 has a table defined. 

When calling (memo-fib 3), E2 is created with n = 3. 
We enter the procedure and as 3 is not in the table, we need 
to call (memo-fib 2) and (memo-fib 1).

Say within this implementation (memo-fib 1) is called first,
we create E3 with n = 1 pointing to E1. 
As 1 is not on the map, we execute the fib func and we get 1 and 
put it on the table and close E3. 

We call (memo-fib 2) E4 pointing to E1 with n = 2. 
As 2 is not on the map, we execute the code and we
call (memo-fib 1) and (memo-fib 0).

We call (memo-fib 0) E5 pointing to E1 with n = 0.
As 0 is not on the map, we execute the fib func and 
we put 0 on the map and close E5.

We call (memo-fib 1) E6 pointing to E1 with n = 1.
As 1 is on the map, we don't execute the fib code 
and directly return 1 and we close E6.

Back to E2, we can compute the result 0 + 1 = 1

The explanation for why we compute fib so fast is that 
when we execute (memo-fib n) we are going to "expand"
all the fibo from (memo-fib (- n 2)), but when computing
(memo-fib (- n 1)), we'll only expand to (memo-fib (- n 2)) and 
(memo-fib (- n 3)) and no more because we'll find the result on 
the table so that (fib (- n 2)) will not be called more than 
once. 

The result is that if we draw it in terms of node (see picture),
we can see that we have 4N calls and no more.

|#


#| Exercice 3.28 |#

(define (or-gate a1 a2 output)
    (define (or-action-procedure)
        (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
            (after-delay or-gate-delay (lambda ()
                                            (set-signal! output new-value)
                                        )
            )
        )
    )
    (set-action! a1 or-action-procedure)
    (set-action! a2 or-action-procedure)
)

(define (logical-or s1 s2)
    (cond ((= 1 s1) 1)
          ((= 1 s2) 1)
          (else 0)
    )
)

#| Exercice 3.29 |#

#| 
A || B = !(!A & !B)
|#

(define (or-gate a1 a2 output)
    (let ((not-a1 (make-wire))
          (not-a2 (make-wire))
          (not-a1-a2 (make-wire))
        )
        (inverter a1 not-a1)
        (inverter a2 not-a2)
        (and-gate not-a1 not-a2 not-a1-a2)
        (inverter not-a1-a2 output)
    )
)

#| 
As the and will be triggered once either a1 
or a2 changes, we'll have 
1 inverter delay + 1 and-gate-delay + 1 inverter-delay
=> 2 inverter delays + and-gate-delay
|#

#| Exercice 3.30 |#

(define (ripple-carry-adder a-list b-list s-list c)
    (cond ((null? a-list) 'ok)
          (else (let ((sum (make-wire))
                      (a (make-wire))
                      (b (make-wire))
                     )
                    (set-signal! a (car a-list))
                    (set-signal! b (car b-list))
                    (adder a b sum c)
                    (set-action! sum (lambda () (set-car! s-list (get-signal sum))))
                    (ripple-carry-adder (cdr a-list) (cdr b-list) (cdr s-list) c)
                )
          )
    )
)

#| 
half-adder => or-delay + max (and-delay, or-delay + inverter)
adder : 2 half-adder + or-delay
ripple-carry-adder : 
n * adder

=> 2n half-adder + n or-delay
=> 2n * (or-delay + max (and-delay, or-delay + inverter)) + n * or-delay
=> 3n * or-delay + 2n * max (and-delay, or-delay + inverter)
|#

#| Exercice 3.31 |#

#| 
Not clear, I don't really understand and the answers on
the site are not clear to me as well
|#

#| Exercice 3.32 |#

#| 

Initial 0, 1
First a1 changes from 0 -> 1 a1's lambda inserted to the q
Second a2 changes from 1 -> 0 a2's lambda inserted to the q

If we are FIFO then:
a2's lambda procedure triggered  => 0
a1's lambda procedure triggered  => 1 

If we are LIFO then 
a1's lambda procedure => 1
a2's lambda procedure => 0

So we need LIFO
|#


#| Exercice 3.33 |#

(define (averager a b c)
    (define (process-new-value)
        (cond ((and (has-value? a) (has-value? b))
               (set-value! c (+ (get-value a) (get-value b)) me)
              )
              ((and (has-value? a) (has-value? c))
               (set-value! b (- (get-value c) (get-value a)))
              )
              ((and (has-value? b) (has-value? c))
               (set-value! a (- (get-value c) (get-value b)))
              )
        )
    )

    (define (process-forget-value)
        (forget-value! a me)
        (forget-value! b me)
        (forget-value! c me)
        (process-new-value)
    )

    (define (me request)
        (cond ((eq? request 'I-have-a-value) process-new-value)
              ((eq? request 'I-lost-my-value) process-forget-value)
              (else (error "Request not supported" request))
        )
    )
    (connect a me)
    (connect b me)
    (connect c me)
    me
)


#| Exercice 3.34 |#

#| 
If a has no value, when we change b, 
nothing is triggered in process-new-value
in the multiplier because it requires 2 out
of the three variable to have a value.
|#

#| Exercice 3.35 |#

(define (squarer a b)
    (define (process-new-value)
        (if (has-value? b)
            (if (< (get-value b) 0)
                (error "square less than 0" (get-value b))
                (set-value! a (sqrt b) me)
            )
            (set-value! b (quare a) me)
        )
    )

    (define (process-forget-value)
        (forget-value! a me)
        (forget-value! b me)
        (process-new-value)
    )

    (define (me request)
        (cond ((eq? request 'I-have-a-value) process-new-value)
              ((eq? request 'I-lost-my-value) process-forget-value)
              (else (error "Request not supported" request))
        )
    )

    (connect a me)
    (connect b me)
    me
)

#| Exercice 3.36 |#

#| Won't do |#


#| Exercice 3.37 |#

(define (c+ a b)
    (let ((c (make-connector)))
        (adder a b c)
        c
    )
)

(define (c* a b)
    (let ((c (make-connector)))
        (multiplier a b c)
        c
    )
)

(define (c- a b)
    (let ((c (make-connector)))
        (adder b c a)
        c
    )
)

(define (c/ a b)
    (let ((c (make-connector)))
        (multiplier b c a)
        c
    )
)

(define (cv val)
    (let ((c (make-connector)))
        (constant val c)
        c
    )
)
#| Exercise 5.7 |#

#| This exercice can't be done yet |#

#| Exercise 5.8 |#

#| 
start 
    (goto (label here))
here
    (assign a (const 3))
    (goto (label there))
here
    (assign a (const 4))
    (goto (label there))
there
|#

#| a. |#

#| 
With the current implementation, 
a would contain 3 because:
- When assembling the instructions, the order 
of the processed instructions will not change,
thus, in the constructed labels, the first 
here will be "before" the second one in the instructions 
list associated with the label here. 
- When calling assoc, we will reach the first "here" 
and execute it, then go to there and 
the second here won't never be reached.
|#

#| b. |#

(define (extract-labels text receive)
    (if (null? text)
        (receive '() '())
        (extract-labels (cdr text)
                (lambda (insts labels)
                    (let ((next-inst (car text)))
                        (if (symbol? next-inst)
                            (if (assoc next-inst labels)
                                (error "Label already exists -- ASSEMBLE" next-int)
                                (receive insts
                                    (cons (make-label-entry next-inst insts)
                                          labels
                                    )
                                )
                            )

                            (receive (cons (make-instruction next-inst) insts)
                                    labels
                            )
                        )
                    )
                )
        )
    )
)

#| Exercise 5.9 |#

(define (make-operation-exp exp machine labels operations)
    (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs (map (lambda (e)
                     (if (label-exp e)
                        (error "Trying to make an operation
                        with label as operand -- ASSEMBLE" e)
                        (make-primitive-exp e machine labels)
                     ))
                    (operation-exp-operands exp)

        )))
        (lambda ()
            apply op (map (lambda (p (p))) aprocs)
        )
    )
)

#| Exercise 5.10 |#

#| 
new-Syntax: 
assign => <-
reg => val
goto => go
branch => if-true
test => try
op => ope
label => block
|#

(define (register-exp? exp)
    (tagged-list? exp 'val)
)

(define (label-exp? exp)
    (tagged-list? exp 'block)
)

(define (operation-exp? exp)
    (and (pair? exp) (tagged-list? (car exp) 'ope))
)

(define (make-execution-procedure inst labels machine pc flag stack ops)
    (cond ((eq? (car inst) '<-)
        (make-assign inst machine labels ops pc)
        )
        ((eq? (car inst) 'try)
        (make-test inst machine labels ops flag pc)
        )
        ((eq? (car inst) 'if-true)
        (make-branch inst machine labels flag pc)
        )
        ((eq? (car inst) 'go)
        (make-goto inst machine labels pc)
        )
        ((eq? (car inst) 'save)
        (make-save inst machine stack pc)
        )
        ((eq? (car inst) 'restore)
        (make-restore inst machine stack pc)
        )
        ((eq? (car inst) 'perform)
        (make-perform inst machine labels ops pc)
        )
        (else 
            (error "Unknown instruction type -- ASSEMBLE" inst)
        )
    )
)

(define gcd-machine
    (make-machine
        '(a b t)
        (list (list 'rem remainder) (list '= =))
        '(test-b
            (try (ope =) (val b) (const 0))
            (if-true (block gcd-done))
            (<- t (ope rem) (val a) (val b))
            (<- a (val b))
            (<- b (val t))
            (go (block test-b))
        gcd-done
        )
    )
)

#| Exercise 5.11 |#

#| a. |#

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
        ;(restore continue) 
        (assign n (op -) (reg n) (const 2))
        ;(save continue)
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

#| b. |#

(define (make-save inst machine stack pc)
    (let ((reg (get-register machine (stack-inst-reg-name inst))))
        (lambda ()
            (push stack (cons (stack-inst-reg-name inst)
                              (get-contents reg)
                        )
            )
            (advance-pc pc)
        )
    )
)

(define (make-restore inst machine stack pc)
    (let ((reg (get-register machine (stack-inst-reg-name inst))))
        (lambda ()
            (let ((stack-top (pop stack)))
                (if (eq? (stack-inst-reg-name inst) (car stack-top))
                    (begin (set-contents! reg (cdr stack-pop))
                        (advance-pc pc)
                    )
                )
                (error "The top of the stack does not match the
                        restore register")
            )
            
        )
    )
)

(define gcd-machine
    (make-machine
        '(a b t)
        (list (list 'rem remainder) (list '= =))
        '(test-b
            (try (ope =) (val b) (const 0))
            (if-true (block gcd-done))
            (save a)
            (restore b)
            (<- t (ope rem) (val a) (val b))
            (<- a (val b))
            (<- b (val t))
            (go (block test-b))
        gcd-done
        )
    )
)

(set-register-contents! gcd-machine 'a 206)

(set-register-contents! gcd-machine 'b 40)


#| c. |#

(define (make-new-machine)
    (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (the-instruction-sequence '())
        )

        (let ((register-table (list (list 'pc pc) (list 'flag flag)))
              (register-stack-table '())
             )
            (define (stack-initialize)
                (for-each 
                    (lambda (name)
                        (set! register-stack-table (cons (list name (make-stack)) 
                        register-stack-table))
                    )
                    (map car register-table)
                )
            )

            (let ((the-ops 
                    (list (list 'initialize-stack (lambda () (stack-initialize)))))
                )
                (define (allocate-register name)
                        (if (assoc name register-table)
                            (error "Multiply defined register: " name)
                            (set! register-table (cons (list name (make-register name)) 
                                                        register-table))
                                

                        )
                        'register-allocated
                )



                (define (lookup-stack name) 
                    (let ((val (assoc name register-stack-table)))
                        (if val
                            (cadr val)
                            (error "Unkown register: " name)
                        )
                    )
                )
                (define (lookup-register name)
                        (let ((val (assoc name register-table)))
                            (if val
                                (cadr val)
                                (error "Unknown reigtser: " name)
                            )
                        )
                )
                (define (execute)
                        (let ((insts (get-contents pc)))
                            (if (null? insts)
                                'done
                                (begin
                                    ((instruction-execution-proc (car insts)))
                                    (execute)
                                )
                            )
                        )
                )

                (define (dispatch message)
                        (cond ((eq? message 'start) 
                            (set-contents! pc the-instruction-sequence)
                            (execute)
                            )
                            ((eq? message 'install-instructions-sequence)
                            (lambda (seq)
                                (set! the-instruction-sequence seq)
                            )
                            )
                            ((eq? message 'allocate-register)
                            allocate-register
                            )
                            ((eq? message 'initialize-stack)
                            (initialize-stack)
                            )
                            ((eq? message 'get-register)
                            lookup-register
                            )
                            ((eq? message 'install-operations)
                            (lambda (ops) (set! the-ops (append the-ops ops)))
                            )
                            ((eq? message 'get-stack)
                                lookup-stack
                            )
                            ((eq? message 'operations)
                            the-ops
                            )
                            ((eq? message 'insts)
                                (get-contents pc)
                            )
                            (else (error "Unknown request -- MACHINE" message))
                        )
                )
                dispatch
            )
        )       
    )
)


(define (update-insts! insts labels machine)
    (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (get-stack (machine 'get-stack))
        (ops (machine 'operations))
        )
        (for-each
            (lambda (inst)
                (set-instruction-execution-proc! 
                    inst
                    (make-execution-procedure
                        (instruction-text inst) labels machine
                        pc flag get-stack ops
                    )
                )
            )
            insts
        )
    )
)

(define (make-execution-procedure inst labels machine pc flag get-stack ops)
    (cond ((eq? (car inst) 'assign)
        (make-assign inst machine labels ops pc)
        )
        ((eq? (car inst) 'test)
        (make-test inst machine labels ops flag pc)
        )
        ((eq? (car inst) 'branch)
        (make-branch inst machine labels flag pc)
        )
        ((eq? (car inst) 'goto)
        (make-goto inst machine labels pc)
        )
        ((eq? (car inst) 'save)
        (make-save inst machine get-stack pc)
        )
        ((eq? (car inst) 'restore)
        (make-restore inst machine get-stack pc)
        )
        ((eq? (car inst) 'perform)
        (make-perform inst machine labels ops pc)
        )
        (else 
            (error "Unknown instruction type -- ASSEMBLE" inst)
        )
    )
)

(define (make-save inst machine get-stack pc)
    (let ((reg (get-register machine (stack-inst-reg-name inst))))
        (lambda ()
            (push (get-stack (stack-inst-reg-name inst))
                  (get-contents reg)
            )
            (advance-pc pc)
        )
    )
)

(define (make-restore inst machine get-stack pc)
    (let ((reg (get-register machine (stack-inst-reg-name inst))))
        (lambda ()
                (set-contents! reg (pop (get-stack (stack-inst-reg-name inst))))
                (advance-pc pc)
        )
    )
)


(define gcd-machine
    (make-machine
        '(a b t)
        (list (list 'rem remainder) (list '= =))
        '(
            (perform (op initialize-stack))
            test-b
                (test (op =) (reg b) (const 0))
                (branch (label gcd-done))
                (assign t (op rem) (reg a) (reg b))
                (assign a (reg b))
                (assign b (reg t))
                (save a)
                (save b)
                (restore a)
                (restore b)
                (goto (label test-b))
            gcd-done
        )
    )
)

#| Exercise 5.12 |#

#| The datastructure storing the first list 
will be a list of pairs which first object the 
instruction type and the second is a list 
with all the corresponding instruction
|#

#| a. |#

(define (eq-instruction? inst1 inst2)
    (cond ((and (null? inst1) (null? inst2)) #t)
          ((and (symbol? inst1) (symbol? inst2)) (eq? inst1 inst2))
          ((and (number? inst1) (number? inst2)) (= inst1 inst2))
          ((and (pair? inst1) (pair? inst2)) (and (eq-instruction? (car inst1) (car inst2))
                                                  (eq-instruction? (cdr inst1) (cdr inst2))))
          (else #f)
    )
)

(define (to-add-no-duplicate? inst the-list)
    (define (iter curr-list)
        (cond ((null? curr-list) 
               #t
              )
              ((eq-instruction? (car curr-list) inst)
               #f
              )
            (else (iter (cdr curr-list)))
        )
    )
    (iter the-list)
)




(define (make-new-machine)
    (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (the-instruction-list '())
        )
        (let ((the-ops 
                (list (list 'initialize-stack (lambda () (stack-initialize)))))
                (register-table (list (list 'pc pc) (list 'flag flag)))
            )
            (define (initialize-instruction-list insts)
                (for-each 
                    insert-in-instruction-list
                    (filter pair? insts)
                )
            )

            (define (insert-in-instruction-list inst)
                (define (iter curr-list)
                    (cond ((null? curr-list) (set! the-instruction-list (cons (cons (car inst) (list inst)) the-instruction-list)))
                          ((eq? (caar curr-list) (car inst))
                           (if (to-add-no-duplicate? inst (cdar curr-list))
                               (set-cdr! (car curr-list) (cons inst (cdar curr-list)))
                               'done
                           )
                          )
                          (else (iter (cdr curr-list)))
                    )
                )
                (iter the-instruction-list)
            )
            (define (allocate-register name)
                    (if (assoc name register-table)
                        (error "Multiply defined register: " name)
                        (set! register-table (cons (list name (make-register name)) 
                                                    register-table))
                    )
                    'register-allocated
            )
            (define (lookup-register name)
                    (let ((val (assoc name register-table)))
                        (if val
                            (cadr val)
                            (error "Unknown reigtser: " name)
                        )
                    )
            )
            (define (execute)
                    (let ((insts (get-contents pc)))
                        (if (null? insts)
                            'done
                            (begin
                                ((instruction-execution-proc (car insts)))
                                (execute)
                            )
                        )
                    )
            )

            (define (dispatch message)
                    (cond ((eq? message 'start) 
                        (set-contents! pc the-instruction-sequence)
                        (execute)
                        )
                        ((eq? message 'install-instructions-sequence)
                        (lambda (seq)
                            (set! the-instruction-sequence seq)
                        )
                        )
                        ((eq? message 'allocate-register)
                        allocate-register
                        )
                        ((eq? message 'get-register)
                        lookup-register
                        )
                        ((eq? message 'install-operations)
                        (lambda (ops) (set! the-ops (append the-ops ops)))
                        )
                        ((eq? message 'stack)
                        stack
                        )
                        ((eq? message 'operations)
                        the-ops
                        )
                        ((eq? message 'initialize-instruction-list)
                            initialize-instruction-list 
                        )
                        ((eq? message 'instruction-list)
                         the-instruction-list
                        )
                        ((eq? message 'insts)
                            (get-contents pc)
                        )
                        (else (error "Unknown request -- MACHINE" message))
                    )
            )
            dispatch
        )       
    )
)

(define (make-machine register-names ops controller-text)
    (let ((machine (make-new-machine)))
        (for-each (lambda (register-name)
                        ((machine 'allocate-register)
                        register-name
                        )
                )
                register-names
        )
        ((machine 'install-operations) ops)
        ((machine 'install-instructions-sequence)
        (assemble controller-text machine)
        )
        ((machine 'initialize-instruction-list) controller-text)
        machine
    )
)



#| b. / c.  |#

#| A list without duplicate of all registers holding entry-points |#

(define (is-in-list? el the-list)
    (cond ((null? the-list) #f)
          ((eq? (car the-list) el) #t)
          (else (is-in-list? el (cdr the-list)))
    )
)


(define (make-new-machine)
    (let ((pc (make-register 'pc))
          (flag (make-register 'flag))
          (stack (make-stack))
          (the-instruction-sequence '())
          (the-instruction-list '())
          (the-entry-points-list '())
          (the-stack-registers-list '())
        )
        (let ((the-ops 
                (list (list 'initialize-stack (lambda () (stack-initialize)))))
                (register-table (list (list 'pc pc) (list 'flag flag)))
            )
            (define (initialize-entry-points-list insts)
                    (let ((goto-insts (filter 
                                            (lambda (inst)
                                                (and (pair? inst) 
                                                    (eq? (car inst) 'goto)
                                                    (eq? (caadr inst) 'reg)
                                                )
                                            )
                                            insts
                                        )
                        ))
                        (for-each
                            (lambda (inst)
                                (let ((reg-name (cadadr inst)))
                                    (if (not (is-in-list? reg-name the-stack-registers-list))
                                        (set! the-entry-points-list (cons reg-name the-entry-points-list))
                                    )
                                )

                            )
                            goto-insts
                        )
                    )
            )

            (define (initialize-stack-registers-list insts)
                (let ((stack-insts (filter 
                                        (lambda (inst)
                                            (and (pair? inst)
                                                 (or (eq? (car inst) 'restore)
                                                     (eq? (car inst) 'save)
                                                
                                                 )
                                            )
                                        )
                                        insts
                                    )
                    ))
                    (for-each
                        (lambda (inst)
                            (let ((reg-name (cadr inst)))
                                (if (not (is-in-list? reg-name the-entry-points-list))
                                    (set! the-stack-registers-list (cons reg-name the-stack-registers-list))
                                )
                            )

                        )
                        stack-insts
                    )
                )
            )


            (define (initialize-instruction-list insts)
                (for-each 
                    insert-in-instruction-list
                    (filter 
                        pair?
                        insts
                    )
                )
            )

            (define (insert-in-instruction-list inst)
                (define (iter curr-list)
                    (cond ((null? curr-list) (set! the-instruction-list (cons (cons (car inst) (list inst)) the-instruction-list)))
                        ((eq? (caar curr-list) (cadr inst))
                        (if (to-add-no-duplicate? inst (cdar curr-list))
                            (set-cdr! (car curr-list) (cons inst (cdar curr-list)))
                            'done
                        )
                        )
                        (else (iter (cdr curr-list)))
                    )
                )
                (iter the-instruction-list)
            )
            (define (allocate-register name)
                    (if (assoc name register-table)
                        (error "Multiply defined register: " name)
                        (set! register-table (cons (list name (make-register name)) 
                                                    register-table))
                    )
                    'register-allocated
            )
            (define (lookup-register name)
                    (let ((val (assoc name register-table)))
                        (if val
                            (cadr val)
                            (error "Unknown reigtser: " name)
                        )
                    )
            )
            (define (execute)
                    (let ((insts (get-contents pc)))
                        (if (null? insts)
                            'done
                            (begin
                                ((instruction-execution-proc (car insts)))
                                (execute)
                            )
                        )
                    )
            )

            (define (dispatch message)
                    (cond ((eq? message 'start) 
                        (set-contents! pc the-instruction-sequence)
                        (execute)
                        )
                        ((eq? message 'install-instructions-sequence)
                        (lambda (seq)
                            (set! the-instruction-sequence seq)
                        )
                        )
                        ((eq? message 'allocate-register)
                        allocate-register
                        )
                        ((eq? message 'get-register)
                        lookup-register
                        )
                        ((eq? message 'install-operations)
                        (lambda (ops) (set! the-ops (append the-ops ops)))
                        )
                        ((eq? message 'stack)
                            stack
                        )
                        ((eq? message 'operations)
                            the-ops
                        )
                        ((eq? message 'initialize-instruction-list)
                            initialize-instruction-list 
                        )
                        ((eq? message 'instruction-list)
                            the-instruction-list
                        )
                        ((eq? message 'initialize-entry-points-list)
                            initialize-entry-points-list 
                        )
                        ((eq? message 'entry-points-list)
                            the-entry-points-list
                        )
                        ((eq? message 'initialize-stack-registers-list)
                            initialize-stack-registers-list 
                        )
                        ((eq? message 'stack-registers-list)
                            the-stack-registers-list
                        )
                        ((eq? message 'insts)
                            (get-contents pc)
                        )
                        (else (error "Unknown request -- MACHINE" message))
                    )
            )
            dispatch
        )       
    )
)

(define (make-machine register-names ops controller-text)
    (let ((machine (make-new-machine)))
        (for-each (lambda (register-name)
                        ((machine 'allocate-register)
                        register-name
                        )
                )
                register-names
        )
        ((machine 'install-operations) ops)
        ((machine 'install-instructions-sequence)
        (assemble controller-text machine)
        )
        ((machine 'initialize-instruction-list) controller-text)
        ((machine 'initialize-entry-points-list) controller-text)
        ((machine 'initialize-stack-registers-list) controller-text)
        machine
    )
)

(define gcd-machine
    (make-machine
        '(a b t d c)
        (list (list 'rem remainder) (list '= =))
        '(test-b
            (test (op =) (reg b) (const 0))
            (branch (label gcd-done))
            (goto (reg b))
            (assign t (op rem) (reg a) (reg b))
            (save t)
            (restore c)
            (save d)
            (assign a (reg b))
            (assign b (reg t))
            (goto (reg a))
        gcd-done
        )
    )
)


#| d. |#

(define (flatten-1 the-list)    
    (if (and (pair? the-list) (null? (cdr the-list)) (pair? (car the-list)))
        (car the-list)
        the-list
    )
)


(define (make-new-machine)
    (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (the-instruction-list '())
        (the-entry-points-list '())
        (the-stack-registers-list '())
        (the-source-registers-list '())
        )
        (let ((the-ops 
                (list (list 'initialize-stack (lambda () (stack-initialize)))))
                (register-table (list (list 'pc pc) (list 'flag flag)))
            )
            (define (initialize-entry-points-list insts)
                    (let ((goto-insts (filter 
                                            (lambda (inst)
                                                (and (pair? inst) 
                                                    (eq? (car inst) 'goto)
                                                    (eq? (caadr inst) 'reg)
                                                )
                                            )
                                            insts
                                        )
                        ))
                        (for-each
                            (lambda (inst)
                                (let ((reg-name (cadadr inst)))
                                    (if (not (is-in-list? reg-name the-stack-registers-list))
                                        (set! the-entry-points-list (cons reg-name the-entry-points-list))
                                    )
                                )

                            )
                            goto-insts
                        )
                    )
            )

            (define (initialize-stack-registers-list insts)
                (let ((stack-insts (filter 
                                        (lambda (inst)
                                            (and (pair? inst)
                                                (or (eq? (car inst) 'restore)
                                                    (eq? (car inst) 'save)
                                                
                                                )
                                            )
                                        )
                                        insts
                                    )
                    ))
                    (for-each
                        (lambda (inst)
                            (let ((reg-name (cadr inst)))
                                (if (not (is-in-list? reg-name the-entry-points-list))
                                    (set! the-stack-registers-list (cons reg-name the-stack-registers-list))
                                )
                            )

                        )
                        stack-insts
                    )
                )
            )


            (define (initialize-instruction-list insts)
                (for-each 
                    insert-in-instruction-list
                    (filter pair? insts)
                )
            )

            (define (insert-in-instruction-list inst)
                (define (iter curr-list)
                    (cond ((null? curr-list) (set! the-instruction-list (cons (cons (car inst) (list inst)) the-instruction-list)))
                        ((eq? (caar curr-list) (car inst))
                        (if (to-add-no-duplicate? inst (cdar curr-list))
                            (set-cdr! (car curr-list) (cons inst (cdar curr-list)))
                            'done
                        )
                        )
                        (else (iter (cdr curr-list)))
                    )
                )
                (iter the-instruction-list)
            )

            (define (initialize-source-registers-list insts)
                (for-each 
                    insert-in-source-registers-list
                    (filter 
                        (lambda (inst)
                            (and (pair? inst)
                                (eq? (car inst) 'assign)
                            )
                        )
                    insts)
                )
            )

            (define (insert-in-source-registers-list inst)
                (define (iter curr-list)
                    (cond ((null? curr-list) (set! the-source-registers-list (cons (cons (cadr inst) (list (flatten-1 (cddr inst)))) the-source-registers-list)))
                        ((eq? (caar curr-list) (cadr inst))
                        (if (to-add-no-duplicate? (flatten-1 (cddr inst)) (cdar curr-list))
                            (set-cdr! (car curr-list) (cons (flatten-1 (cddr inst)) (cdar curr-list)))
                            'done
                        )
                        )
                        (else (iter (cdr curr-list)))
                    )
                )
                (iter the-source-registers-list)
            )


            (define (allocate-register name)
                    (if (assoc name register-table)
                        (error "Multiply defined register: " name)
                        (set! register-table (cons (list name (make-register name)) 
                                                    register-table))
                    )
                    'register-allocated
            )
            (define (lookup-register name)
                    (let ((val (assoc name register-table)))
                        (if val
                            (cadr val)
                            (error "Unknown reigtser: " name)
                        )
                    )
            )
            (define (execute)
                    (let ((insts (get-contents pc)))
                        (if (null? insts)
                            'done
                            (begin
                                ((instruction-execution-proc (car insts)))
                                (execute)
                            )
                        )
                    )
            )

            (define (dispatch message)
                    (cond ((eq? message 'start) 
                        (set-contents! pc the-instruction-sequence)
                        (execute)
                        )
                        ((eq? message 'install-instructions-sequence)
                        (lambda (seq)
                            (set! the-instruction-sequence seq)
                        )
                        )
                        ((eq? message 'allocate-register)
                        allocate-register
                        )
                        ((eq? message 'get-register)
                        lookup-register
                        )
                        ((eq? message 'install-operations)
                        (lambda (ops) (set! the-ops (append the-ops ops)))
                        )
                        ((eq? message 'stack)
                            stack
                        )
                        ((eq? message 'operations)
                            the-ops
                        )
                        ((eq? message 'initialize-instruction-list)
                            initialize-instruction-list 
                        )
                        ((eq? message 'instruction-list)
                            the-instruction-list
                        )
                        ((eq? message 'initialize-entry-points-list)
                            initialize-entry-points-list 
                        )
                        ((eq? message 'entry-points-list)
                            the-entry-points-list
                        )
                        ((eq? message 'initialize-stack-registers-list)
                            initialize-stack-registers-list 
                        )
                        ((eq? message 'stack-registers-list)
                            the-stack-registers-list
                        )
                        ((eq? message 'initialize-source-registers-list)
                            initialize-source-registers-list 
                        )
                        ((eq? message 'source-registers-list)
                            the-source-registers-list
                        )
                        ((eq? message 'insts)
                            (get-contents pc)
                        )
                        (else (error "Unknown request -- MACHINE" message))
                    )
            )
            dispatch
        )       
    )
)

(define (make-machine register-names ops controller-text)
    (let ((machine (make-new-machine)))
        (for-each (lambda (register-name)
                        ((machine 'allocate-register)
                        register-name
                        )
                )
                register-names
        )
        ((machine 'install-operations) ops)
        ((machine 'install-instructions-sequence)
        (assemble controller-text machine)
        )
        ((machine 'initialize-instruction-list) controller-text)
        ((machine 'initialize-entry-points-list) controller-text)
        ((machine 'initialize-stack-registers-list) controller-text)
        ((machine 'initialize-source-registers-list) controller-text)
        machine
    )
)

(define gcd-machine
    (make-machine
        '(a b t)
        (list (list 'rem remainder) (list '= =))
        '(test-b
            (test (op =) (reg b) (const 0))
            (branch (label gcd-done))
            (assign t (op rem) (reg a) (reg b))
            (assign a (reg b))
            (assign b (reg t))
            (assign a (reg b))
            (assign t (op rem) (reg a) (reg b))
            (goto (label test-b))
        gcd-done
        )
    )
)

#| 
We could probably refactor the code 
so that it is much cleaner 
(basically parsing only once or twice the 
instructions instead of having as parsings as
we have lists to build)
|#

#| Exercise 5.13 |#

(define (make-machine ops controller-text)
    (let ((machine (make-new-machine)))
        ((machine 'install-operations) ops)
        ((machine 'install-instructions-sequence)
        (assemble controller-text machine)
        )
        machine
    )
)

#| 
We'll add a first parsing in 
assemble to create the registers
|#


(define (make-new-machine)
    (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        )
        (let ((the-ops 
                (list (list 'initialize-stack (lambda () (stack-initialize)))))
                (register-table (list (list 'pc pc) (list 'flag flag)))
            )
            (define (allocate-register name)
                    (if (assoc name register-table)
                        (error "Multiply defined register: " name)
                        (set! register-table (cons (list name (make-register name)) 
                                                    register-table))
                    )
                    'register-allocated
            )

            (define (exists-register? name)
                (let ((val (assoc name register-table)))
                            (if val
                                #t
                                #f
                            )
                ) 
            )
            (define (lookup-register name)
                    (let ((val (assoc name register-table)))
                        (if val
                            (cadr val)
                            (error "Unknown regitser: " name)
                        )
                    )
            )
            (define (execute)
                    (let ((insts (get-contents pc)))
                        (if (null? insts)
                            'done
                            (begin
                                ((instruction-execution-proc (car insts)))
                                (execute)
                            )
                        )
                    )
            )

            (define (dispatch message)
                    (cond ((eq? message 'start) 
                        (set-contents! pc the-instruction-sequence)
                        (execute)
                        )
                        ((eq? message 'install-instructions-sequence)
                        (lambda (seq)
                            (set! the-instruction-sequence seq)
                        )
                        )
                        ((eq? message 'allocate-register)
                        allocate-register
                        )
                        ((eq? message 'get-register)
                        lookup-register
                        )
                        ((eq? message 'install-operations)
                        (lambda (ops) (set! the-ops (append the-ops ops)))
                        )
                        ((eq? message 'stack)
                        stack
                        )
                        ((eq? message 'operations)
                        the-ops
                        )
                        ((eq? message 'exists-register?)
                        exists-register?
                        )
                        ((eq? message 'insts)
                            (get-contents pc)
                        )
                        (else (error "Unknown request -- MACHINE" message))
                    )
            )
            dispatch
        )       
    )
)

(define (assemble controller-text machine)
    (begin 
        (extract-registers 
            controller-text 
            machine
        )
        (extract-labels controller-text
                        (lambda (insts labels)
                            (update-insts! insts labels machine)
                            insts
                        )
        ))
)

(define (extract-registers controller-text machine)
    (for-each 
        (lambda (inst)
            (extract-register inst machine)
        )
        controller-text
    )
)

(define (extract-register inst machine)
    (cond ((not (pair? inst)) '())
          ((eq? (car inst) 'assign)
           (extract-reg-assign inst machine)
          )
          ((eq? (car inst) 'test)
           (extract-reg-test inst machine)
          )
          ((eq? (car inst) 'branch)
           '()
          )
          ((eq? (car inst) 'goto)
          (extract-reg-goto inst machine)
          )
          ((eq? (car inst) 'save)
          (extract-reg-save inst machine)
          )
          ((eq? (car inst) 'restore)
          (extract-reg-restore inst machine)
          )
          ((eq? (car inst) 'perform)
          (extract-reg-perform inst machine)
          )
          (else 
          (error "Unknown instruction type -- ASSEMBLE" inst)
          )
    )
)

(define (is-reg? exp)
    (eq? (car exp) 'reg)
)

(define (extract-reg-test inst machine)
    (let ((test-operands (cddr inst)))
        (for-each
            (lambda (operand)
                (if (not ((machine 'exists-register?) (cadr operand)))
                    ((machine 'allocate-register)
                        (cadr operand)
                    )
                )
            )
            (filter 
                is-reg? 
                test-operands
            )
        )
    )
)

(define (extract-reg-goto inst machine)
    (if (and (is-reg? (cadr inst)) (not ((machine 'exists-register? (cadadr inst)))))
        ((machine 'allocate-register)
            (cadadr inst)
        )
    )
)

(define (extract-reg-save inst machine)
    (if (not ((machine 'exists-register? (cadr inst))))
        ((machine 'allocate-register)
         (cadr inst)
        )
    )
)

(define (extract-reg-restore inst machine)
    (if (not ((machine 'exists-register? (cadr inst))))
        ((machine 'allocate-register)
        (cadr inst)
        )
    )
)

(define (extract-reg-perform inst machine)
    (let ((perform-operands (cddr inst)))
        (for-each
            (lambda (operand)
                (if (not ((machine 'exists-register?) (cadr operand)))
                    ((machine 'allocate-register)
                        (cadr operand)
                    )
                )
            )
            (filter 
                is-reg? 
                perform-operands
            )
        )
    )
)

(define (extract-reg-assign inst machine)

    (let ((reg-name (cadr inst))
          (source (caddr inst))
         )
        (begin 
            (if (not ((machine 'exists-register?) reg-name))
                ((machine 'allocate-register)
                 reg-name
                )
            )
            (if (and (is-reg? source) (not ((machine 'exists-register?) (cadr source))))
                ((machine 'allocate-register)
                 (cadr source)
                )
            )
        )

    )
)



(define gcd-machine
    (make-machine
        (list (list 'rem remainder) (list '= =))
        '(test-b
            (test (op =) (reg b) (const 0))
            (branch (label gcd-done))
            (assign t (op rem) (reg a) (reg b))
            (assign a (reg b))
            (assign b (reg t))
            (goto (label test-b))
        gcd-done
        )
    )
)

#| Exercise 5.14 |#

(define factorial-machine
    (make-machine
        '(n continue val)
        (list (list '* *) 
              (list '= =) 
              (list 'read read)
              (list '- -)
              (list 'print (lambda (value) (newline) (display value) (newline)))
        )
        '(read-loop
              (perform (op initialize-stack))
              (assign n (op read))
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
            (perform (op print) (reg val))
            (perform (op print-stack-statistics))
            (goto (label read-loop))
        )
    )
)

#| 
The formula is as follows: 
number-pushes = n * 2 - 2
max-depth = n * 2 - 2
|#

#| Exercise 5.15 |#

(define (make-new-machine)
    (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (num-instructions-executed 0)
        )
        (define (print-statistics)
            (newline)
            (display (list 'num-instructions '= num-instructions-executed))
            (newline)
        )
        (let ((the-ops 
                (list (list 'initialize-stack (lambda () (stack 'initialize)))
                      (list 'print-stack-statistics (lambda () (stack 'print-statistics)))
                      (list 'initialize-statistics (lambda () (set! num-instructions-executed 0)))
                      (list 'print-statistics print-statistics)
                ))
                (register-table (list (list 'pc pc) (list 'flag flag)))
            )
            (define (allocate-register name)
                    (if (assoc name register-table)
                        (error "Multiply defined register: " name)
                        (set! register-table (cons (list name (make-register name)) 
                                                    register-table))
                    )
                    'register-allocated
            )
            (define (lookup-register name)
                    (let ((val (assoc name register-table)))
                        (if val
                            (cadr val)
                            (error "Unknown reigtser: " name)
                        )
                    )
            )
            (define (execute)
                    (let ((insts (get-contents pc)))
                        (if (null? insts)
                            'done
                            (begin
                                ((instruction-execution-proc (car insts)))
                                (execute)
                            )
                        )
                    )
            )

            (define (dispatch message)
                    (cond ((eq? message 'start) 
                        (set-contents! pc the-instruction-sequence)
                        (execute)
                        )
                        ((eq? message 'install-instructions-sequence)
                        (lambda (seq)
                            (set! the-instruction-sequence seq)
                        )
                        )
                        ((eq? message 'allocate-register)
                        allocate-register
                        )
                        ((eq? message 'get-register)
                        lookup-register
                        )
                        ((eq? message 'install-operations)
                        (lambda (ops) (set! the-ops (append the-ops ops)))
                        )
                        ((eq? message 'stack)
                        stack
                        )
                        ((eq? message 'operations)
                        the-ops
                        )
                        ((eq? message 'increment-instructions!)
                         (set! num-instructions-executed (+ 1 num-instructions-executed))
                        )
                        ((eq? message 'trace?)
                         trace? 
                        )
                        ((eq? message 'insts)
                            (get-contents pc)
                        )
                        (else (error "Unknown request -- MACHINE" message))
                    )
            )
            dispatch
        )       
    )
)



(define (trace? machine)
    (machine 'trace?)
)

(define (increment-instructions! machine)
    (machine 'increment-instructions!)

)

(define (make-assign inst machine labels operations pc)
    (let ((target (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst))
        )
        (let ((value-proc (if (operation-exp? value-exp)
                                (make-operation-exp
                                    value-exp machine labels operations
                                )
                                (make-primitive-exp
                                    (car value-exp) machine labels
                                )
                            )
            )
            )
            (lambda ()
                (set-contents! target (value-proc))
                (increment-instructions! machine)
                (advance-pc pc)
            )
        )
   
 ))

(define (make-test inst machine labels operations flag pc)
    (let ((condition (test-condition inst)))
        (if (operation-exp? condition)
            (let ((condition-proc 
                    (make-operation-exp
                        condition machine labels operations
                    )
                ))
                (lambda ()
                    (set-contents! flag (condition-proc))
                    (increment-instructions! machine)
                    (advance-pc pc)
                )
            )
            (error "Bad TEST instruction -- ASSEMBLE" inst)
        )
    )
)

(define (make-branch inst machine labels flag pc)
    (let ((dest (branch-dest inst)))
        (if (label-exp? dest)
            (let ((insts (lookup-label labels (label-exp-label dest))))
                (lambda ()
                    (if (get-contents flag)
                            (set-contents! pc insts)
                        (advance-pc pc)
                    )
                    (increment-instructions! machine)
                )
            )
            (error "Bad BRANCH instruction -- ASSEMBLE" inst)
        )
    )
)

(define (make-goto inst machine labels pc)
    (let ((dest (goto-dest inst)))
        (cond ((label-exp? dest)
            (let ((insts (lookup-label labels (label-exp-label dest))))
                    (lambda () (set-contents! pc insts))
                )
            )
            ((register-exp? dest)
            (let ((reg (get-register machine (register-exp-reg dest))))
                    (lambda ()
                        (set-contents! pc (get-contents reg))
                        (increment-instructions! machine)
                    )
            )
            )
            (else (error "Bad GOTO instruction -- ASSEMBLE" inst))
        )
    )
)

(define (make-save inst machine stack pc)
    (let ((reg (get-register machine (stack-inst-reg-name inst))))
        (lambda ()
            (push stack (get-contents reg))
            (advance-pc pc)
            (increment-instructions! machine)
        )
    )
)

(define (make-restore inst machine stack pc)
    (let ((reg (get-register machine (stack-inst-reg-name inst))))
        (lambda ()
            (set-contents! reg (pop stack))
            (advance-pc pc)
            (increment-instructions! machine)
        )
    )
)

(define (make-perform inst machine labels operations pc)
    (let ((action (perform-action inst)))
        (if (operation-exp? action)
            (let ((action-proc (make-operation-exp action machine labels operations)))
                (lambda ()
                    (action-proc)
                    (advance-pc pc)
                    (increment-instructions! machine)
                )
            )
            (error "Bad PERFORM instruction -- ASSEMBLE" inst)
        )
    )
)

(define factorial-machine
    (make-machine
        '(n continue val)
        (list (list '* *) 
            (list '= =) 
            (list 'read read)
            (list '- -)
            (list 'print (lambda (value) (newline) (display value) (newline)))
        )
        '(read-loop
            (perform (op initialize-statistics))
            (perform (op initialize-stack))
            (assign n (op read))
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
            (perform (op print) (reg val))
            (perform (op print-stack-statistics))
            (perform (op print-statistics))
            (goto (label read-loop))
        )
    )
)

#| 
Would have been easier to put everything in the execute 
procedure.
This is what we'll do now.

|#

#| Exercise 5.16 |#


(define (make-new-machine)
    (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (num-instructions-executed 0)
        (trace? #t)
        )
        (define (print-statistics)
            (newline)
            (display (list 'num-instructions '= num-instructions-executed))
            (newline)
        )
        (define (set-trace-on!)
            (set! trace? #t)
        )
        (define (set-trace-off!)
            (set! trace? #f)
        )
        (let ((the-ops 
                (list (list 'initialize-stack (lambda () (stack 'initialize)))
                    (list 'print-stack-statistics (lambda () (stack 'print-statistics)))
                    (list 'initialize-statistics (lambda () (set! num-instructions-executed 0)))
                    (list 'print-statistics print-statistics)
                    (list 'trace-on set-trace-on!)
                    (list 'trace-off set-trace-off!)
                ))
                (register-table (list (list 'pc pc) (list 'flag flag)))
            )
            (define (allocate-register name)
                    (if (assoc name register-table)
                        (error "Multiply defined register: " name)
                        (set! register-table (cons (list name (make-register name)) 
                                                    register-table))
                    )
                    'register-allocated
            )
            (define (lookup-register name)
                    (let ((val (assoc name register-table)))
                        (if val
                            (cadr val)
                            (error "Unknown reigtser: " name)
                        )
                    )
            )
            (define (execute)
                    (let ((insts (get-contents pc)))
                        (if (null? insts)
                            'done
                            (begin
                                ((instruction-execution-proc (car insts)))
                                (if trace?
                                    (begin 
                                        (newline)
                                        (display (instruction-text (car insts)))
                                    )
                                )
                                (execute)
                            )
                        )
                    )
            )

            (define (dispatch message)
                    (cond ((eq? message 'start) 
                        (set-contents! pc the-instruction-sequence)
                        (execute)
                        )
                        ((eq? message 'install-instructions-sequence)
                        (lambda (seq)
                            (set! the-instruction-sequence seq)
                        )
                        )
                        ((eq? message 'allocate-register)
                        allocate-register
                        )
                        ((eq? message 'get-register)
                        lookup-register
                        )
                        ((eq? message 'install-operations)
                        (lambda (ops) (set! the-ops (append the-ops ops)))
                        )
                        ((eq? message 'stack)
                        stack
                        )
                        ((eq? message 'operations)
                        the-ops
                        )
                        ((eq? message 'increment-instructions!)
                        (set! num-instructions-executed (+ 1 num-instructions-executed))
                        )
                        ((eq? message 'trace?)
                        trace? 
                        )
                        ((eq? message 'insts)
                            (get-contents pc)
                        )
                        (else (error "Unknown request -- MACHINE" message))
                    )
            )
            dispatch
        )       
    )
)

#| Trace on |#

(define factorial-machine
    (make-machine
        '(n continue val)
        (list (list '* *) 
            (list '= =) 
            (list 'read read)
            (list '- -)
            (list 'print (lambda (value) (newline) (display value) (newline)))
        )
        '(read-loop
            (perform (op initialize-statistics))
            (perform (op initialize-stack))
            (assign n (op read))
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
            (perform (op print) (reg val))
            (perform (op print-stack-statistics))
            (perform (op print-statistics))
            (goto (label read-loop))
        )
    )
)

#| Trace off |#

(define factorial-machine
    (make-machine
        '(n continue val)
        (list (list '* *) 
            (list '= =) 
            (list 'read read)
            (list '- -)
            (list 'print (lambda (value) (newline) (display value) (newline)))
        )
        '(read-loop
            (perform (op trace-off))
            (perform (op initialize-statistics))
            (perform (op initialize-stack))
            (assign n (op read))
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
            (perform (op print) (reg val))
            (perform (op print-stack-statistics))
            (perform (op print-statistics))
            (goto (label read-loop))
        )
    )
)

#| Exercise 5.17 |#

(define (make-new-machine)
    (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (num-instructions-executed 0)
        (trace? #t)
        )
        (define (print-statistics)
            (newline)
            (display (list 'num-instructions '= num-instructions-executed))
            (newline)
        )
        (define (set-trace-on!)
            (set! trace? #t)
        )
        (define (set-trace-off!)
            (set! trace? #f)
        )
        (let ((the-ops 
                (list (list 'initialize-stack (lambda () (stack 'initialize)))
                    (list 'print-stack-statistics (lambda () (stack 'print-statistics)))
                    (list 'initialize-statistics (lambda () (set! num-instructions-executed 0)))
                    (list 'print-statistics print-statistics)
                    (list 'trace-on set-trace-on!)
                    (list 'trace-off set-trace-off!)
                ))
                (register-table (list (list 'pc pc) (list 'flag flag)))
            )
            (define (allocate-register name)
                    (if (assoc name register-table)
                        (error "Multiply defined register: " name)
                        (set! register-table (cons (list name (make-register name)) 
                                                    register-table))
                    )
                    'register-allocated
            )
            (define (lookup-register name)
                    (let ((val (assoc name register-table)))
                        (if val
                            (cadr val)
                            (error "Unknown reigtser: " name)
                        )
                    )
            )
            (define (execute)
                    (let ((insts (get-contents pc)))
                        (if (null? insts)
                            'done
                            (begin
                                ((instruction-execution-proc (car insts)))
                                (if trace?
                                    (begin 
                                        (newline)
                                        (display 
                                            (list 'instructions '= (instruction-text (car insts))
                                                  'label '= (instruction-label (car insts))
                                            )
                                    
                                        )
                                    )
                                )
                                (execute)
                            )
                        )
                    )
            )

            (define (dispatch message)
                    (cond ((eq? message 'start) 
                        (set-contents! pc the-instruction-sequence)
                        (execute)
                        )
                        ((eq? message 'install-instructions-sequence)
                        (lambda (seq)
                            (set! the-instruction-sequence seq)
                        )
                        )
                        ((eq? message 'allocate-register)
                        allocate-register
                        )
                        ((eq? message 'get-register)
                        lookup-register
                        )
                        ((eq? message 'install-operations)
                        (lambda (ops) (set! the-ops (append the-ops ops)))
                        )
                        ((eq? message 'stack)
                        stack
                        )
                        ((eq? message 'operations)
                        the-ops
                        )
                        ((eq? message 'increment-instructions!)
                        (set! num-instructions-executed (+ 1 num-instructions-executed))
                        )
                        ((eq? message 'trace?)
                        trace? 
                        )
                        ((eq? message 'insts)
                            (get-contents pc)
                        )
                        ((eq? message 'insts-seq)
                            the-instruction-sequence
                        )
                        (else (error "Unknown request -- MACHINE" message))
                    )
            )
            dispatch
        )       
    )
)



(define (assemble controller-text machine)
    (extract-labels controller-text
                    (lambda (insts labels)
                        (update-insts! insts labels machine)
                        insts
                    )
                    'no-label
    )
)


(define (extract-labels text receive curr-label)
    (if (null? text)
        (receive '() '())
        (let ((next-curr-label (if (symbol? (car text))
                                    (car text)
                                    curr-label
                                )
              ))
            (extract-labels 
                (cdr text)
                (lambda (insts labels)
                    (let ((next-inst (car text)))
                        (if (symbol? next-inst)
                            (receive insts
                                    (cons (make-label-entry next-inst insts)
                                        labels
                                    )
                            )
                            (receive (cons (make-instruction next-inst curr-label) insts)
                                    labels
                            )
                        )
                    )
                )
                next-curr-label
            )
    
        )
    )
)

(define (set-instruction-execution-proc! inst proc)
    (set-cdr! (cdr inst) (cons proc '()))
)

(define (make-instruction text label)
    (list text label)
)

(define (instruction-label inst)
    (cadr inst)
)

(define (instruction-execution-proc inst)
    (caddr inst)
)

 
(define factorial-machine
    (make-machine
        '(n continue val)
        (list (list '* *) 
            (list '= =) 
            (list 'read read)
            (list '- -)
            (list 'print (lambda (value) (newline) (display value) (newline)))
        )
        '(read-loop
            (perform (op initialize-statistics))
            (perform (op initialize-stack))
            (assign n (op read))
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
            (perform (op print) (reg val))
            (perform (op print-stack-statistics))
            (perform (op print-statistics))
            (goto (label read-loop))
        )
    )
)

#| Exercise 5.18 |#

(define (make-register name)
    (let ((contents '*unassigned)
          (trace? #f)
         )
        (define (dispatch message)
            (cond ((eq? message 'get) contents)
                ((eq? message 'set) 
                    (lambda (value)
                        (if trace?
                            (begin (newline)
                            (display (list 'register name
                                        'old-value  contents
                                        'new-value value
                                    )
                            ))
                        )
                        (set! contents value)
                    ) 
                )
                ((eq? message 'trace-on)
                 (set! trace? #t)
                )
                ((eq? message 'trace-off)
                 (set! trace? #f)
                )
                (else 
                    (error "Unknown request -- REGISTER" message)    
                )
            )   
        )
        dispatch
    )
)

(define (make-new-machine)
    (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (num-instructions-executed 0)
        (trace? #t)
        )
        (define (print-statistics)
            (newline)
            (display (list 'num-instructions '= num-instructions-executed))
            (newline)
        )
        (define (set-trace-on!)
            (set! trace? #t)
        )
        (define (set-trace-off!)
            (set! trace? #f)
        )
        (let (
                (register-table (list (list 'pc pc) (list 'flag flag)))
            )
            (define (trace-register name)
                (let ((val (assoc name register-table)))
                    (if val
                        ((cadr val) 'trace-on)
                        (error "Unknown reigtser: " name)
                    )
                )
            )
            (define (untrace-register name)
                (let ((val (assoc name register-table)))
                    (if val
                        ((cadr val) 'trace-off)
                        (error "Unknown reigtser: " name)
                    )
                )
            )
            (let ((the-ops 
                    (list (list 'initialize-stack (lambda () (stack 'initialize)))
                    (list 'print-stack-statistics (lambda () (stack 'print-statistics)))
                    (list 'initialize-statistics (lambda () (set! num-instructions-executed 0)))
                    (list 'print-statistics print-statistics)
                    (list 'trace-on set-trace-on!)
                    (list 'trace-off set-trace-off!)
                    (list 'trace-register trace-register)
                    (list 'untrace-register untrace-register)
            )))
        
                (define (allocate-register name)
                        (if (assoc name register-table)
                            (error "Multiply defined register: " name)
                            (set! register-table (cons (list name (make-register name)) 
                                                        register-table))
                        )
                        'register-allocated
                )
                (define (lookup-register name)
                        (let ((val (assoc name register-table)))
                            (if val
                                (cadr val)
                                (error "Unknown reigtser: " name)
                            )
                        )
                )
                (define (execute)
                        (let ((insts (get-contents pc)))
                            (if (null? insts)
                                'done
                                (begin
                                    ((instruction-execution-proc (car insts)))
                                    (if trace?
                                        (begin 
                                            (newline)
                                            (display 
                                                (list 'instructions '= (instruction-text (car insts))
                                                    'label '= (instruction-label (car insts))
                                                )
                                        
                                            )
                                        )
                                    )
                                    (execute)
                                )
                            )
                        )
                )

                (define (dispatch message)
                        (cond ((eq? message 'start) 
                            (set-contents! pc the-instruction-sequence)
                            (execute)
                            )
                            ((eq? message 'install-instructions-sequence)
                            (lambda (seq)
                                (set! the-instruction-sequence seq)
                            )
                            )
                            ((eq? message 'allocate-register)
                            allocate-register
                            )
                            ((eq? message 'get-register)
                            lookup-register
                            )
                            ((eq? message 'install-operations)
                            (lambda (ops) (set! the-ops (append the-ops ops)))
                            )
                            ((eq? message 'stack)
                            stack
                            )
                            ((eq? message 'operations)
                            the-ops
                            )
                            ((eq? message 'increment-instructions!)
                            (set! num-instructions-executed (+ 1 num-instructions-executed))
                            )
                            ((eq? message 'trace?)
                            trace? 
                            )
                            ((eq? message 'insts)
                                (get-contents pc)
                            )
                            ((eq? message 'insts-seq)
                                the-instruction-sequence
                            )
                            (else (error "Unknown request -- MACHINE" message))
                        )
                )
                dispatch
            )       
        )
    )
)

(define (make-primitive-exp exp machine labels)
    (cond 
        ((constant-exp? exp)
        (let ((c (constant-exp-value exp)))
            (lambda () c)
        )
        )
        ((symbol? exp)
         (lambda () exp)
        )
        ((label-exp? exp)
        (let ((insts (lookup-label labels (label-exp-label exp))))
            (lambda () insts)
        )
        )
        ((register-exp? exp)
        (let ((r (get-register machine (register-exp-reg exp))))
                (lambda () (get-contents r))
        )
        )
        (else 
            (error "Unknown expression type -- ASSEMBLE" exp)
        )

    )
)

(define factorial-machine
    (make-machine
        '(n continue val)
        (list (list '* *) 
            (list '= =) 
            (list 'read read)
            (list '- -)
            (list 'print (lambda (value) (newline) (display value) (newline)))
        )
        '(read-loop
            (perform (op trace-register) n)
            (perform (op initialize-statistics))
            (perform (op initialize-stack))
            (assign n (op read))
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
            (perform (op untrace-register) n)
            (restore n)
            (restore continue)
            (assign val (op *) (reg val) (reg n))
            (goto (reg continue))
        base-case
            (assign val (const 1))
            (goto (reg continue))
        fact-done
            (perform (op print) (reg val))
            (perform (op print-stack-statistics))
            (perform (op print-statistics))
            (goto (label read-loop))
        )
    )
)

#| Exercise 5.19 |#

#| 
We'll do this exercise with the structure of instructions that we build
in Exercise 5.17

|#

(define (make-new-machine)
    (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (num-instructions-executed 0)
        (trace? #t)
        (is-in-breakpoint (cons #f '()))
        )
        (define (print-statistics)
            (newline)
            (display (list 'num-instructions '= num-instructions-executed))
            (newline)
        )
        (define (set-trace-on!)
            (set! trace? #t)
        )
        (define (set-trace-off!)
            (set! trace? #f)
        )
        (let (
                (register-table (list (list 'pc pc) (list 'flag flag)))
            )
            (define (trace-register name)
                (let ((val (assoc name register-table)))
                    (if val
                        ((cadr val) 'trace-on)
                        (error "Unknown reigtser: " name)
                    )
                )
            )
            (define (untrace-register name)
                (let ((val (assoc name register-table)))
                    (if val
                        ((cadr val) 'trace-off)
                        (error "Unknown reigtser: " name)
                    )
                )
            )
            (let ((the-ops 
                    (list (list 'initialize-stack (lambda () (stack 'initialize)))
                    (list 'print-stack-statistics (lambda () (stack 'print-statistics)))
                    (list 'initialize-statistics (lambda () (set! num-instructions-executed 0)))
                    (list 'print-statistics print-statistics)
                    (list 'trace-on set-trace-on!)
                    (list 'trace-off set-trace-off!)
                    (list 'trace-register trace-register)
                    (list 'untrace-register untrace-register)
            )))
        
                (define (allocate-register name)
                        (if (assoc name register-table)
                            (error "Multiply defined register: " name)
                            (set! register-table (cons (list name (make-register name)) 
                                                        register-table))
                        )
                        'register-allocated
                )
                (define (lookup-register name)
                        (let ((val (assoc name register-table)))
                            (if val
                                (cadr val)
                                (error "Unknown reigtser: " name)
                            )
                        )
                )
                (define (execute)
                        (let ((insts (get-contents pc)))
                            (cond ((null? insts) 'done)
                                  ((car is-in-breakpoint)
                                    (begin
                                      (newline)
                                      (display (list 'breakpoint
                                            'label '= (instruction-label (car insts))
                                            'offset '= (cdr is-in-breakpoint)
                                      ))
                                    )
                                  )
                                  (else 
                                    (begin
                                        ((instruction-execution-proc (car insts)))
                                        (if trace?
                                            (begin 
                                                (newline)
                                                (display 
                                                    (list 'instructions '= (instruction-text (car insts))
                                                        'label '= (instruction-label (car insts))
                                                    )
                                            
                                                )
                                            )
                                        )
                                        (execute)
                                    )
                                  )

                            )
                        )
                )

                (define (set-breakpoint! label n)
                    (define (iter curr-list counter)
                        (cond ((null? curr-list)
                               (error "This position in the instruction does not exist")
                              )
                              ((and (eq? (instruction-label (car curr-list)) label) (= counter 0))
                                (let ((curr-inst (car curr-list))
                                        (next-insts (cdr curr-list))
                                    )
                        
                                    (set-car! curr-list
                                        (list 'breakpoint label 
                                            (lambda ()
                                                (set! is-in-breakpoint (cons #t n))
                                            )
                                        )
                                    )
                                    (set-cdr! curr-list
                                        (cons curr-inst
                                                next-insts
                                        )
                                    )
                                )   
                              )
                              ((eq? (instruction-label (car curr-list)) label)
                               (iter (cdr curr-list) (- counter 1))
                              )
                              (else (iter (cdr curr-list) counter))
                        )
                    )
                    (iter the-instruction-sequence (- n 1))
                )


                (define (proceed-machine)
                    (set! is-in-breakpoint (cons #f '()))
                    (advance-pc pc)
                    (execute)
                )

                (define (cancel-breakpoint! label n)
                    (define (iter curr-list counter)
                        (cond ((null? curr-list)
                            (error "This position in the instruction does not exist")
                            )
                            ((and (eq? (instruction-label (car curr-list)) label) (= counter 0))
                                (let ((next-insts (cdr curr-list)))
                        
                                    (set-car! curr-list
                                        (cadr curr-list)
                                    )
                                    (set-cdr! curr-list
                                        (cddr curr-list)
                                    )
                                )   
                            )
                            ((eq? (instruction-label (car curr-list)) label)
                             (iter (cdr curr-list) (- counter 1))
                            )
                            (else (iter (cdr curr-list) counter))
                        )
                    )
                    (iter the-instruction-sequence (- n 1))
                )

                (define (cancel-all-breakpoints!)
                    (define (iter curr-list)
                        (cond ((null? curr-list)
                               'done
                              )
                              ((eq? (instruction-text (car curr-list)) 'breakpoint)
                              (let ((next-insts (cdr curr-list)))
                                  (set-car! curr-list
                                      (cadr curr-list)
                                  )
                                  (set-cdr! curr-list
                                      (cddr curr-list)
                                  )
                                  (iter (cdr curr-list))
                              )   
                              )
                              (else (iter (cdr curr-list)))
                        )
                    ) 
                    (iter the-instruction-sequence)
                )

                (define (dispatch message)
                        (cond ((eq? message 'start) 
                            (set-contents! pc the-instruction-sequence)
                            (execute)
                            )
                            ((eq? message 'install-instructions-sequence)
                            (lambda (seq)
                                (set! the-instruction-sequence seq)
                            )
                            )
                            ((eq? message 'allocate-register)
                            allocate-register
                            )
                            ((eq? message 'get-register)
                            lookup-register
                            )
                            ((eq? message 'install-operations)
                            (lambda (ops) (set! the-ops (append the-ops ops)))
                            )
                            ((eq? message 'stack)
                            stack
                            )
                            ((eq? message 'operations)
                            the-ops
                            )
                            ((eq? message 'increment-instructions!)
                            (set! num-instructions-executed (+ 1 num-instructions-executed))
                            )
                            ((eq? message 'trace?)
                            trace? 
                            )
                            ((eq? message 'insts)
                                (get-contents pc)
                            )
                            ((eq? message 'insts-seq)
                                the-instruction-sequence
                            )
                            ((eq? message 'set-breakpoint!)
                                set-breakpoint!
                            )
                            ((eq? message 'proceed-machine)
                                (proceed-machine)
                            )
                            ((eq? message 'cancel-breakpoint!)
                             cancel-breakpoint!
                            )
                            ((eq? message 'cancel-all-breakpoints!)
                             (cancel-all-breakpoints!)
                            )
                            (else (error "Unknown request -- MACHINE" message))
                        )
                )
                dispatch
            )       
        )
    )
)

(define (set-breakpoint machine label n)
    ((machine 'set-breakpoint!) label n)
)

(define (proceed-machine machine)
    (machine 'proceed-machine)
)

(define (cancel-breakpoint machine label n)
    ((machine 'cancel-breakpoint!) label n)
)

(define (cancel-all-breakpoints machine)
    (machine 'cancel-all-breakpoints!)
)


(define factorial-machine
    (make-machine
        '(n continue val)
        (list (list '* *) 
            (list '= =) 
            (list 'read read)
            (list '- -)
            (list 'print (lambda (value) (newline) (display value) (newline)))
        )
        '(read-loop
            (perform (op trace-off))
            (perform (op initialize-statistics))
            (perform (op initialize-stack))
            (assign n (op read))
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
            (perform (op print) (reg val))
            (goto (label read-loop))
        )
    )
)

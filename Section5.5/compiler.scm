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

(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements)
)

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '())
)

(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence
            '(continue) '()
            '((goto (reg continue)))
         )
        )
        ((eq? linkage 'next)
         (empty-instruction-sequence)
        )
        (else 
          (make-instruction-sequence '()
                                     '()
                                     `((goto (label ,linkage)))
          )
        )
  )
)

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
              instruction-sequence
              (compile-linkage linkage)
  )
)

(define (compile-self-evaluating exp target linkage)
  (end-with-linkage linkage
    (make-instruction-sequence '()
                               (list target)
                               `((assign ,target (const ,exp)))
    ) 
  )
)

(define (compile-quoted exp target linkage)
  (end-with-linkage linkage
    (make-instruction-sequence '()
                              (list target)
                              `((assign ,target (const ,(text-of-quotation exp))))
    ) 
  )
)

(define (compile-variable exp target linkage)
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
)

(define (compile-assignment exp target linkage)
  (let ((var (assignment-variable exp))
        (get-value-code 
              (compile (assignment-value exp) 'val 'next)
        )
       )
      (end-with-linkage linkage
        (preserving '(env)
                    get-value-code
                    (make-instruction-sequence 
                        '(env val)
                         (list target)
                         `((perform (op set-variable-value!)
                                    (const ,var)
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

(define (compile-definition exp target linkage)
  (let ((var (definition-variable exp))
        (get-value-code 
              (compile (definition-value exp) 'val 'next)
        )
        )
        (end-with-linkage linkage
          (preserving '(env)
                      get-value-code
                      (make-instruction-sequence 
                          '(env val)
                          (list target)
                          `((perform (op define-variable!)
                                      (const ,var)
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

(define label-counter 0)

(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter
)

(define (make-label name)
  (string->symbol
    (string-append (symbol->string name)
                   (number->string (new-label-number))
    )
  )
)

(define (compile-if exp target linkage)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if))
       )
       (let ((consequent-linkage
              (if (eq? linkage 'next) after-if linkage)
             )
            )
            (let ((p-code (compile (if-predicate exp) 'val 'next))
                  (c-code (compile (if-consequent exp) target consequent-linkage))
                  (a-code (compile (if-alternative exp) target linkage))
                 )
                 (preserving 
                    '(env continue)
                    p-code
                    (append-instruction-sequences
                      (make-instruction-sequence 
                        '(val) 
                        '()
                        `((test (op false?) (reg val))
                          (branch (label ,f-branch))
                         )
                      )
                      (parallel-instruction-sequences
                        (append-instruction-sequences t-branch c-code)
                        (append-instruction-sequences f-branch a-code)
                      )
                      after-if
                    )
                 )
            )
       )
  )
)

(define (compile-sequence seq target linkage)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage)
      (preserving 
          '(env continue)
          (compile (first-exp seq) target 'next)
          (compile-sequence (rest-exps seq) target linkage)
      )
  )
)

(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env)
)

(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure)
)

(define (compiled-procedure-entry c-proc)
  (cadr c-proc)
)

(define (compiled-procedure-env c-proc) 
  (caddr c-proc)
)

(define (compile-lambda exp target linkage)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda))
       )
       (let ((lambda-linkage (if (eq? linkage 'next) after-lambda linkage))
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
                (compile-lambda-body exp proc-entry)
              )
              after-lambda
            )
       )
  ) 
)

(define (compile-lambda-body exp proc-entry)
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
          (compile-sequence (lambda-body exp) 'val 'return)
        )
  )
)

(define (compile-application exp target linkage)
  (let ((proc-code (compile (operator exp) 'proc 'next))
        (operand-codes
          (map (lambda (operand) (compile operand 'val 'next))
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

(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes))
       )
       (if (null? operand-codes)
           (make-instruction-sequence 
                '()
                '(argl)'
                '((assign argl (const ())))
           )
           (let ((code-to-get-last-arg 
                  (append-instruction-sequences
                    (car operand-codes)
                    (make-instruction-sequence '(val) '(argl)
                      '((assign argl (op list) (reg val)))
                    )
                  )
                ))
                (if (null? (cdr operand-codes))
                    code-to-get-last-arg
                    (preserving '(env)
                                code-to-get-last-arg
                                (code-to-get-rest-args
                                  (cdr operand-codes)
                                )
                    )
                )
           )
       )
  )
)


(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg (preserving '(argl)
                              (car operand-codes)
                              (make-instruction-sequence '(val argl)
                                  '(argl)
                                  '((assign argl
                                      (op cons)
                                      (reg val)
                                      (reg argl)
                                   ))
                              )
                           ))
       )
       (if (null? (cdr operand-codes))
           code-for-next-arg
           (preserving '(env)
                       code-for-next-arg
                       (code-to-get-rest-args (cdr operands-code))
           )
       )
  )
)

(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call))
       )
       (let ((compiled-linkage 
              (if (eq? linkage 'next)
                  after-call
                  linkage
              )
            ))
            (append-instruction-sequences
              (make-instruction-sequence
                '(proc)
                '()
                `((test (op primitive-procedure?) (reg proc))
                  (branch (label ,primitive-branch))
                 )
              )
              (parallel-instruction-sequences
                (append-instruction-sequences
                  compiled-branch
                  (compile-proc-appl target compiled-linkage)
                )
                (append-instruction-sequences
                  primitive-branch
                  (end-with-linkage linkage
                    (make-instruction-sequence '(proc argl)
                      (list target)
                      `((assign ,target
                                (op apply-primitive-procedure)
                                (reg proc)
                                (reg argl)
                    
                        ))
                    )
                  )
                )
              )
              after-call
            )
        )
  ) 
)

(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return))) 
        (make-instruction-sequence '(proc)
              all-regs
              `((assign continue (label ,linkage))
                (assign val (op compiled-procedure-entry)
                            (reg proc)
                )
                (goto (reg val))
              )
        )
        )
        ((and (not (eq? target 'val)) (not (eq? linkage 'return)))
        (let ((proc-return (make-label 'proc-return)))
            (make-instruction-sequence
                '(proc)
                all-regs
                `((assign continue (label ,proc-return))
                  (assign val (op compiled-procedure-entry)
                              (reg proc)
                  )
                  (goto (reg val))
                  ,proc-return
                  (assign ,target (reg val))
                  (goto (label ,linkage))
                )
            )
        )
        )
        ((and (eq? target 'val) (eq? linkage 'return))
        (make-instruction-sequence 
            '(proc continue)
            all-regs
            '((assign val (op compiled-procedure-entry) 
                          (reg proc)
              )
              (goto (reg val))
            )
        )
        )
        ((and (not (req? target 'val)) (eq? linkage 'return))
        (error "return linkage, target not val -- COMPILE" target)
        )
  )
)

(define (registers-needed s)
  (if (symbol? s) '() (car s))
)

(define (registers-modified s)
  (if (symbol? s) '() (cadr s))
)

(define (statements s)
  (if (symbol? s) (list s) (caddr s))
)

(define (needs-register? seq reg)
  (memq reg (registers-needed seq))
)

(define (modifies-register? seq reg)
  (memq reg (registers-modified seq))
)

(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
      (list-union 
        (registers-needed seq1)
        (list-difference (registers-needed seq2) (registers-needed seq1))
      )
      (list-union
        (registers-modified seq1)
        (list-difference (registers-modified seq2) (registers-modified seq1))
      )
      (append (statements seq1) (statements seq2))
    )
  )

  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences
          (car seqs)
          (append-seq-list (cdr seqs))
        )
    )
  )

  (append-seq-list seqs)
)

(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))
  )
)

(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1) (list-difference (cdr s1) s2)))
  )
)

(define (preserving regs seq1 seq2)
    (if (null? regs)
        (append-instruction-sequences seq1 seq2)
        (let ((first-reg (car regs)))
          (if (and (needs-register? seq2 first-reg) 
                   (modifies-register? seq1 first-reg)
              )
              (preserving (cdr regs)
                (make-instruction-sequence
                  (list-union (list first-reg)
                              (registers-needed seq1)
                  )
                  (list-difference (registers-modified seq1)
                                    (list first-reg)
                  )
                  (append 
                    `((save ,first-reg))
                    (statements seq1)
                    `((restore ,first-reg))
                  )
                )
                seq2
              )
              (preserving (cdr regs)
                seq1 
                seq2
              )
          )
        )
    )
)


(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
    (registers-needed seq)
    (registers-modified seq)
    (append (statements seq) (statements body-seq))
  )
)


(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
    (list-union (registers-needed seq1)
                (registers-needed seq2)
    )
    (list-union (registers-modified seq1)
                (registers-modified seq2)
    )
    (append (statements seq1) (statements seq2))
  )
)

(define all-regs '(env proc val argl continue))



(define eceval
  (make-machine
      '(exp env val proc argl continue unev)
      eceval-operations
      '(
      (branch (label external-entry))
      read-eval-print-loop
          (perform (op initialize-stack))
          (perform 
              (op prompt-for-input)
              (const "::: EC-Eval input: ")
          )
          (assign exp (op read))
          (assign env (op get-global-environment))
          (assign continue (label print-result))
          (goto (label eval-dispatch))
      
      external-entry
        (perform (op initialize-stack))
        (assign env (op get-global-environment))
        (assign continue (label print-result))
        (goto (reg val))
      
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
          (test (op compiled-procedure?) (reg proc))
          (branch (label compiled-apply))
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
      
      compiled-apply
          (restore continue)
          (assign val (op compiled-procedure-entry) (reg proc))
          (goto (reg val))

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
        (list 'compiled-procedure? compiled-procedure?)
        (list 'compiled-procedure-entry compiled-procedure-entry)
        (list 'compiled-procedure-env compiled-procedure-env)
        (list '= =)
        (list '* *)
        (list '+ +)
  )
)



(define (empty-arglist) '())

(define (adjoin-arg arg arglist)
  (append arglist (list arg))
) 

(define (last-operand? ops)
  (null? (cdr ops))
)

(define (get-global-environment) the-global-environment)

(define (start-eceval)
  (set! the-global-environment (setup-environment))
  (set-register-contents! eceval 'flag #f)
  (start eceval)
)

(define (user-print object)
  (cond ((compound-procedure? object)
         (display (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>
          ))
        )
        ((compiled-procedure? object)
         (display '<compiled-procedure>)
        )
        (else (display object))
  )
)

(define (compile-and-go expression)
  (let ((instructions 
          (assemble (statements
            (compile expression 'val 'return')
          ) eceval)
        ))
        (set! the-global-environment (setup-environment))
        (set-register-contents! eceval 'val instructions)
        (set-register-contents! eceval 'flag #t)
        (start eceval)
  )
)


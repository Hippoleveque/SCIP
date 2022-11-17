(define (require p)
    (if (not p)
        (amb)
    )
)

(define (an-element-of items)
    (require (not (null? items)))
    (amb (car items) (an-element-of (cdr items)))
)

(define (an-integer-starting-from n)
    (amb n (an-integer-starting-from (+ n 1)))
)

(define (distinct? items)
    (cond ((null? items) #t)
          ((null? (cdr items)) #t)
          ((member (car items) (cdr items)) #f)
          (else (distinct? (cdr items)))
    )
)

(define (multiple-dwelling)
    (let ((baker (amb 1 2 3 4 5))
          (cooper (amb 1 2 3 4 5))
          (fletcher (amb 1 2 3 4 5))
          (miller (amb 1 2 3 4 5))
          (smith (amb 1 2 3 4 5))
         )
         (require 
            (distinct (list baker cooper fletcher miller smith))
         )
         (require 
            (not (= baker 5))
         )
         (require 
            (not (= cooper 1))
         )
         (require
            (not (= fletcher 1))
         )
         (require
            (not (= fletcher 5))
         )
         (require
            (> miller cooper)
         )
        (require (not (= (abs (- smith fletcher)) 1)))
        (require (not (= (abs (- fletcher cooper)) 1)))
        (list (list 'baker baker)
              (list 'cooper cooper)
              (list 'fletcher fletcher)
              (list 'miller miller)
              (list 'smith smith)
        )
    )
)

(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))

(define (parse-sentence)
    (list 'sentence
          (parse-noun-phrase)
          (parse-word verbs)
    )
)

(define (parse-noun-phrase)
    (list 'noun-phrase
          (parse-word articles)
          (parse-word nouns)
    )
)

(define (parse-word word-list)
    (require (not (null? *unparsed*)))
    (require (memq (car *unparsed*) (cdr word-list)))
    (let ((found-word (car *unparsed*)))
        (set! *unparsed* (cdr *unparsed*))
        (list (car word-list) found-word)
    )
)

(define *unparsed* '())

(define (parse input)
    (set! *unparsed* input)
    (let ((sent (parse-sentence)))
        (require (null? *unparsed*))
        sent
    )
)

(define prepositions '(prep for to in by with))

(define (parse-prepositional-phrase)
    (list 'prep-phrase
          (parse-word prepositions)
          (parse-noun-phrase)
    )
)

(define (parse-sentence)
    (list 'sentence
          (parse-noun-phrase)
          (parse-verb-phrase)
    )
)

(define (parse-verb-phrase)
    (define (maybe-extend verb-phrase)
        (amb verb-phrase 
             (maybe-extend (list 'verb-phrase
                                 verb-phrase
                                 (parse-prepositional-phrase)
                            )
            )
        )
    )
    (maybe-extend (parse-word verbs))
)

(define (parse-simple-noun-phrase)
    (list 'simple-noun-phrase
          (parse-word articles)
          (parse-word nouns)
    )
)


(define (parse-noun-phrase)
    (define (maybe-extend noun-phrase)
        (amb noun-phrase 
            (maybe-extend (list 'noun-phrase
                                noun-phrase
                                (parse-prepositional-phrase)
                            )
            )
        )
    )
    (maybe-extend (parse-simple-noun-phrase verbs))
)

(define (amb? exp)
    (tagged-list? 'amb exp)
)

(define (amb-choices exp)
    (cdr exp)
)

(define (analyze exp)
    (cond ((self-evaluating? exp) 
        (analyze-self-evaluating exp))
        ((quoted? exp)
        (analyze-quoted exp)
        )
        ((variable? exp)
        (analyze-variable exp)
        )
        ((assignment? exp)
        (analyze-assignment exp)
        )
        ((definition? exp)
        (analyze-definition exp)
        )
        ((if? exp)
        (analyze-if exp)
        )
        ((lambda? exp)
        (analyze-lambda exp)
        )
        ((begin? exp)
        (analyze-sequence (begin-actions exp))
        )
        ((cond? exp)
        (analyze (cond->if exp))
        )
        ((amb? exp) (analyze-amb exp))
        ((application? exp)
        (analyze-application exp)
        )
        (else (error "Unknown expression type -- ANALYZE" exp))
    )
)

(define (ambeval exp env succeed fail)
    ((analyze exp) env succeed fail)
)

(define (analyze-self-evaluating exp)
    (lambda (env succeed fail)
        (succeed exp fail)
    )
)

(define (analyze-quoted exp)
    (let ((qval (text-of-quotation exp)))
        (lambda (env succeed fail) (succeed qval fail))
    )
)

(define (analyze-variable exp)
    (lambda (env succeed fail) 
        (succeed (lookup-variable-value exp env) fail)
    )
)

(define (analyze-lambda exp)
    (let ((vars (analyze (lambda-parameters exp)))
          (bproc (analyze-sequence (lambda-body exp)))
         )
         (lambda (env succeed fail) 
            (succeed (make-procedure vars bproc env)
                     fail
            )
        )
    )
)

(define (analyze-if exp)
    (let ((pproc (analyze (if-predicate exp)))
          (cproc (analyze (if-consequent exp)))
          (aproc (analyze (if-alternative exp)))
          )
          (lambda (env succeed fail) 
              (pproc env 
                    (lambda (pred-value fail2)
                            (if (true? pred-value)
                                (cproc env succeed fail2)
                                (aproc env suceed fail2)
                            )
                    )
                    fail
              )
          )
    )
)

(define (analyze-sequence exp)
    (define (sequentially proc1 proc2)
        (lambda (env succeed fail) 
                (proc1 env 
                       (lambda (res fail2)
                            (proc2 env succeed fail2)
                       )
                       fail
                )
        )
    )
    (define (loop first-proc rest-procs)
        (if (null? rest-procs)
            first-proc
            (loop (sequentially first-proc (car rest-procs)) 
                (cdr rest-procs)
            )
        )
    )
    (let ((procs (map analyze exp)))
        (if (null? procs)
            (error "Empty sequence -- ANALYZE")
            (loop (car procs) (cdr procs))
        )
    )
)

(define (analyze-definition exp)
    (let ((var (assignment-variable exp))
          (vproc (analyze (assignment-value exp)))
        )
        (lambda (env succeed fail) 
            (vproc env
                   (lambda (val fail2)
                        (define-variable! var val env)
                        (succeed 'ok fail2)
                   )
                  fail
            )
        )
    )
)

(define (analyze-assignment exp)
    (let ((var (assignment-variable exp))
          (vproc (analyze (assignment-value exp)))
         )
         (lambda (env succeed fail)
                  (vproc env
                          (lambda (val fail2)
                              (let ((old-value (lookup-variable-value var env)))
                                    (set-variable-value! var val env)
                                    (succeed 'ok
                                            (lambda ()
                                                (set-variable-value! var old-value env)
                                                (fail2)
                                            )
                                    )
                              )
                          )
                          fail
                  )
         )

    )
)

(define (analyze-application exp)
    (let ((fproc (analyze (operator exp)))
          (aprocs (map analyze (operands exp)))
         )
        (lambda (env suceed fail)
            (fproc env
                   (lambda (val fail2)
                        (get-args aprocs
                                  env
                                  (lambda (args fail3)
                                    (execute-application
                                            proc
                                            args
                                            succeed
                                            fail3    
                                    )
                                  )
                                  fail2
                        )
                   )
                  fail
            )
        )
    )
)

(define (get-args aprocs env succeed fail)
    (if (null? aprocs)
        (succeed '() fail)
        ((car aprocs) env
                (lambda (val fail2)
                    (get-args (cdr aprocs)
                              env
                              (lambda (args fail3)
                                 (succeed (cons arg args) fail3)
                              )
                             fail2
                    )
                    
                )
                fail
        )
    )
)

(define (execute-application proc args succeed fail)
    (cond ((primitive-procedure? proc) 
           (succeed (apply-primitive-procedure proc args)
                    fail
           )
          )
            ((compound-procedure? proc)
                ((procedure-body proc)
                    (extend-environment 
                        (procedure-parameters proc)
                        args
                        (procedure-environment proc)
                    )
                    succeed
                    fail 
                )
            )
        (else (error "Unknown procedure type -- Execute-Application" proc))
    )
)

(define (analyze-amb exp)
    (let ((cprocs (map analyze (amb-choices))))
         (lambda (env succeed fail)
            (define (try-next choices)
                (if (null? choices)
                    (fail)
                    ((car choices)
                          env
                          succeed
                          (lambda ()
                                (try-next (cdr choices))
                          )
                    )
                )
            )
            (try-next cprocs)
         )
    )
)

(define input-prompt "::: Amb-Eval input:")
(define output-prompt "::: Amb-Eval value:")

(define (driver-loop)
    (define (internal-loop try-again)
        (prompt-for-input input-prompt)
        (let ((input (read)))
            (if (eq? input 'try-again)
                (try-again)
                (begin 
                    (newline)
                    (display "::: Starting a new problem ")
                    (ambeval input
                             the-global-environment
                             (lambda (val next-alternative)
                                    (announce-output output-prompt)
                                    (user-print val)
                                    (internal-loop next-alternative)
                             )
                             (lambda ()
                                (announce-output "::: There are no more values of")
                                (user-print input)
                                (driver-loop)
                             )
                    )
                )
            )
        )
    )
    (internal-loop
        (lambda ()
            (newline)
            (display "::: There is no current problem")
            (driver-loop)
        )
    )
)


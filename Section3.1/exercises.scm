#| Exercice 3.1 |#

(define (make-accumulator initial)
    (lambda (amount)
        (set! initial (+ amount initial))
        initial
    )
)

#| Exercice 3.2 |#

(define (make-monitored fn)
    (let ((count 0))
        (define (mf arg)
            (cond ((and (symbol? arg) (eq? arg 'how-many-calls?)) count)
                  ((and (symbol? arg) (eq? arg 'reset-count)) (set! count 0) count)
                  (else (set! count (+ count 1)) (fn arg))
            )
        )
        mf
    )
)

#| Exercice 3.3 |#


(define (make-account balance secret-password)

    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount)) balance)
            (error "Insufficient funds")
        )
    )
    (define (deposit amount)
        (set! balance (+ balance amount))
        balance
    )

    (define (dispatch password op)
        (cond ((not (eq? password secret-password)) (lambda (amount) (newline) (display "Bad Password") (newline)))
              ((eq? op 'withdraw) withdraw)
              ((eq? op 'deposit) deposit)
              (else (error "Operation not supported" op) (newline))
        )
    )
    dispatch
)

#| Exercice 3.4 |#

(define (call-the-cops)
    (newline)
    (display "The cops are coming")
    (newline)
)

(define (make-account balance secret-password)

    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount)) balance)
            (error "Insufficient funds")
        )
    )
    (define (deposit amount)
        (set! balance (+ balance amount))
        balance
    )
    (let ((count-bad-password 0))
        (define (handle-bad-password amount)
            (if (= 7 count-bad-password)
                (call-the-cops)
                (begin 
                (newline)
                (display "Bad password")
                (newline))
            )
        )
        (define (dispatch password op)
            (cond ((not (eq? password secret-password)) (begin (set! count-bad-password (+ count-bad-password 1)) handle-bad-password))
                ((eq? op 'withdraw) (begin (set! count-bad-password 0) withdraw))
                ((eq? op 'deposit) (begin (set! count-bad-password 0) deposit))
                (else (error "Operation not supported" op) (newline))
            )
        )
        dispatch
    )
)

#| Exercice 3.5 |#

(define (random-in-range low high)
    (let ((range (- high low)))
        (+ low (random range))
    )
)


(define (estimate-integral predicate x1 x2 y1 y2 trials)
    (define (test)
        (predicate (random-in-range x1 x2) (random-in-range y1 y2))
    )
    (let ((rect-area (* (- x2 x1) (- y2 y1))))
        (* (monte-carlo trials test) rect-area)
    )
)

(define (P x y)
    (> 1 (sqrt (+ (square x) (square y))))
)

#| Exercice 3.6 |#

(define (make-rand rand-initial)
    (let ((x rand-initial))
        (define (generate)
            (set! x (rand-update x))
            x
        )
        (define (reset new-val)
            (set! x new-val)
        )
        (define (dispatch op)
            (cond ((eq? op 'generate) generate)
                  ((eq? op 'reset) reset)
                  (else (error "Operation not supported"))
            )
        )
        dispatch
    )
)

(define rand (make-rand 19))

#| Exercice 3.7 |#

(define (make-account balance secret-password)

    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount)) balance)
            (error "Insufficient funds")
        )
    )
    (define (deposit amount)
        (set! balance (+ balance amount))
        balance
    )

    (define (joint new-password)
        (lambda (input-password op) 
            (if (eq? input-password new-password)
                (dispatch secret-password op)   
                (lambda (amount) (newline) (display "Bad Password") (newline))
            )
        )
    )


    (define (dispatch password op)
        (cond ((not (eq? password secret-password)) (lambda (amount) (newline) (display "Bad Password") (newline)))
            ((eq? op 'withdraw) withdraw)
            ((eq? op 'deposit) deposit)
            ((eq? op 'joint) joint)
            (else (error "Operation not supported" op) (newline))
        )
    )
    dispatch
)

(define (make-joint account old-password new-password)
    ((account old-password 'joint) new-password)
)

#| Exercice 3.8 |#

#| Ugly solution  |#

(define (make-f)
    (let ((prev-val (- 1)))
        (lambda (val)
            (let ((ex prev-val))
                (set! prev-val val)
                (cond  ((= ex (- 1)) 1)
                       ((= ex 0) (- 1))
                       ((= ex 1) 0)
                )
            )
            
        )
    )
)

(define f (make-f))


#| Exercice 3.9 |#

#| Easy, done on a notebook |#
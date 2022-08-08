#| First, using globally available variables |#


(define balance 100)

(define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        (error "Insufficient funds")
    )
)

#| Second, using encapsulation |#

(define withdraw
    (let ((balance 100))
        (lambda (amount) 
            (if (>= balance amount)
                (begin (set! balance (- balance amount)) balance)
                (error "Insufficient funds")
            )
        )
    )
)

#| Third, making a withdraw processor |#

(define (make-withdraw balance)
    (lambda (amount) 
        (if (>= balance amount)
            (begin (set! balance (- balance amount)) balance)
            (error "Insufficient funds")
        )
    )
)

#| Fourth, creating an account object that can both 
withdraw and deposit money
|#

(define (make-account balance)
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
    (define (dispatch op)
        (cond ((eq? op 'withdraw) withdraw)
              ((eq? op 'deposit) deposit)
              (else (error "Operation not supported" op))
        )
    )
    dispatch
)


#| With assignement and encapsulation |#

(define (rand)
    (let ((x random-init))
        (lambda () (set! x (rand-update x)) x)
    )
)

(define (estimate-pi trials )
    (sqrt (/ 6 (monte-carlo trials cesaro-test)))
)

(define (cesaro-test)
    (= (gcd (rand) (rand)) 1)
)

(define (monte-carlo trials experiment)
    (define (iter remaining-trials trials-passed)
        (cond  ((= 0 remaining-trials) (/ trials-passed trials))
               ((experiment) (iter (- remaining-trials 1) (+ trials-passed 1)))
               (else (iter (- remaining-trials 1) trials-passed))
        )
    )
    (iter trials 0)
)

#| Without assigment and encapsulation |#

(define (estimate-pi trials)
    (sqrt (/ 6 (random-gcd-test trials random-init)))
)

(define (random-gcd-test trials initial-x)
    (define (iter reamaining-trials trials-passed x)
        (let ((x1 (rand-update x)))
            (let ((x2 (rand-update x1)))
                (cond ((= remaining-trials 0) (/ trials-passed trials))
                      ((= 1 (gcd x1 x2)) (iter (- remaining-trials 1) (+ trials-passed 1) x2))
                      (else (iter (- remaining-trials 1) trials-passed x2))
                )
        
            )
    
        )

    )
)

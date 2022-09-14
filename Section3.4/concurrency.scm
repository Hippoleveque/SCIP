(define (make-account balance)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                   balance)
            "Insufficient funds"
        )
    )
    (define (deposit amount)
        (begin (set! balance (+ balance account))
                balance
        )
    )
    (let ((protected (make-serializer)))
        (define (dispatch m)
            (cond ((eq? m 'withdraw) (protected withdraw))
                  ((eq? m 'deposit) (protected deposit))
                  ((eq? m 'balance) balance)
                  (else (error "Unsupported request" m))
            )
        )
        dispatch
    )   
)

(define (make-account balance)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount)) balance)
        )
    )

    (define (deposit amount)
        (begin (set! balance (+ balance amont)) balance)
    )

    (let ((balance-serializer (make-serializer)))
        (define (dispatch m)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  ((eq? m 'balance) balance)
                  ((eq? m 'serializer) balance-serializer)
                  (else (error "Unsupported request" m))
            )
        )
    )
)

(define (deposit amount account)
    (((account 'serializer) (account 'deposit)) amount)
)

(define (exchange account1 account2)
    (let ((difference (- account1 account2)))
        ((account1 'withdraw) difference)
        ((account2 'deposit) difference)
    )
)


(define (serialized-exchange account1 account2)
    (let ((serializer1 (acount1 'serializer))
        (serializer2 (account2 'serializer))
        (serializer1 (serializer2 exchange)) account1 
        account2)
    )
)

(define (make-serializer)
    (let ((mutex (make-mutex)))
        (lambda (p) 
            (define (serialized-p . args)
                (mutex 'acquire)
                (let ((val (apply p args)))
                    (mutex 'release)
                    val
                )
            )
            (serialized-p)
        )
    )
)

(define (make-mutex)
    (let ((mutex (list false)))
        (define (the-mutex m)
            (cond ((eq? m 'acquire)
                    (if (test-and-set! mutex)
                        (the-mutex 'acquire)
                    )
                  )
                  ((eq? m 'release)
                   (clear! mutex)
                  )
            )
        )
    )
)

(define (clear! cell)
    (set-car! cell false)
)

(define (test-and-set! cell)
    (if (car cell)
        true
        (begin (set-car! cell true) false)
    )
)
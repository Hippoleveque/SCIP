(define (naive-fibonacci n)
    (if (< n 2) 1 (+ (naive-fibonacci (- n 1)) (naive-fibonacci (- n 2))))
)

(define (iterative-fibonacci n)
    (define (iter a b counter)
        (if (> counter n) b (iter b (+ a b) (+ counter 1)))
    )
    (iter 1 1 3)
)

(define (count-change amount)
    (define (first-denom kinds-of-coins)
        (cond ((= kinds-of-coins 1) 1)
              ((= kinds-of-coins 2) 5)
              ((= kinds-of-coins 3) 10)
              ((= kinds-of-coins 4) 25)
              ((= kinds-of-coins 5) 50)
        )
    )
    (define (cc amount kinds-of-coins)
        (cond ((= amount 0) 1)
              ((or (< amount 0) (= 0 kinds-of-coins)) 0)
              (else (+ 
                        (cc amount (- kinds-of-coins 1)) 
                        (cc (- amount (first-denom kinds-of-coins)) kinds-of-coins)
                    )
              )
        )
    )
    (cc amount 5)
)
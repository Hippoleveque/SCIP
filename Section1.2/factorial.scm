(define (recursive-factorial n)
    (if (= n 1) 1 (* n (recursive-factorial (- n 1))))
)

(define (iterative-factorial n)
    (define (iter current count)
        (if (> count n) current (iter (* count current) (+ count 1)))
    )
    (iter 1 1)
)

(define (iter-fact current count n)
    (if (> count n) current (iter (* count current) (+ count 1)))
)
(define (gcd a b)
    (if (= 0 b) a (gcd b (remainder a b)))
)

(define (square a)
    (* a a)
)

(define (divide? a b)
    (= 0 (remainder b a))
)

(define (even? a)
    (= 0 (remainder a 2))
)


(define (smallest-divisor n)
    (define (iter candidate)
        (cond ((> (square candidate) n) n)
              ((= 0 (remainder n candidate)) candidate)
              (else (iter (+ candidate 1)))
        )
    )
    (iter 2)
)

(define (is-prime? n)
    (= n (smallest-divisor n))
)


(define (expmod base exp m)
    (cond ((= 0 exp) 1)
          ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
          (else (remainder (* base (expmod base (- exp 1) m)) m))
    )
)

(define (fermat-test n)
    (define (try a)
        (= a (expmod a n n))
    )
    (try (+ (random (- n 1)) 1))
)

(define (fast-prime n times)
    (cond ((= 0 times) #t)
          ((fermat-test n) (fast-prime n (- times 1)))
          (else #f)
    )
)
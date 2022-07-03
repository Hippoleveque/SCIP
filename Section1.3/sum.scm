(define (sum term a next b)
    (if (< b a) 0
        (+ (term a) (sum term (next a) next b))
    )
)

(define (simple-sum a b)
    (define (identity x) x)
    (define (inc x) (+ x 1))
    (sum identity a inc b)
)

(define (sum-cube a b)
    (define (cube x) (* x x x))
    (define (inc x) (+ x 1))
    (sum cube a inc b)
)

(define (sum-pi a b)
    (define (pi-term x) (/ 1.0 (* x (+ x 2))))
    (define (pi-next x) (+ 4 x))
    (sum pi-term a pi-next b)
)

(define (integral f a b dx)
    (define (integral-term x) (f (+ x (/ dx 2))))
    (define (integral-next x) (+ x dx))
    (* dx (sum integral-term a integral-next b))
)
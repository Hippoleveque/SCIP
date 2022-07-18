(define (add-rat x y)
    (make-rat (+ (* (num x) (denom y)) (* (num y) (denom x))) (* (denom x) (denom y)))
)

(define (sub-rat x y)
    (make-rat (- (* (num x) (denom y)) (* (num y) (denom x))) (* (denom x) (denom y)))
)

(define (mul-rat x y) 
    (make-rat (* (num x) (num y)) (* (denom x) (denom y)))
)

(define (div-rat x y)
    (make-rat (* (num x) (denom y)) (* (denom x) (num y)))
)

(define (equal-rat? x y)
    (= (* (num x) (denom y)) (* (num y) (denom x)))
)

(define (make-rat a b)
    (let ((g (gcd a b)))
        (cons (/ a g) (/ b g))
    )
)

(define (num x)
    (car x)
)

(define (denom x)
    (cdr x)
)

(define (print-rat x)
    (display (num x))
    (display " / ")
    (display (denom x))
    (newline)
)
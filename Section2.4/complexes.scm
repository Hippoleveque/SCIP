(define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2)) 
                         (+ (imag-part z2) (imag-part z2))
    )
)

(define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2)) 
                         (- (imag-part z2) (imag-part z2))
    ) 
)

(define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))

    )
)

(define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
        (- (angle z1) (angle z2))
    )
)

#| Representing complex in rectangular form |#

(define (real-part z)
    (car z)
)

(define (imag-part z)
    (cdr z)
)   

(define (magnitude z)
    (sqrt (+ (square (real-part z)) (square (imag-part z))))
)

(define (angle z)
    (atan (imag-part z) (real-part z))
)

(define (make-from-real-imag x y)
    (cons x y)
)

(define (make-from-mag-ang r theta)
    (cons (* r (cos theta)) (* r (sin theta)))
)

#| Representing complex in polar form |#

(define (magnitude z)
    (car z)
)

(define (angle z)
    (cdr z)
)

(define (real-part z)
    (* (magnitude z) (cos (angle z)))
)

(define (imag-part z)
    (* (magnitude z) (sin (angle z)))
)

(define (make-from-mag-ang r theta)
    (cons r theta)
)

(define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y))) (atan y x))
)

(define (attach-tag type-tag contents)
    (cons type-tag contents)
)

(define (type-tag datum)
    (if (pair? datum)
        (car datum)
        (error "Bad tagged datum" datum)
    )
)

(define (contents datum)
    (if (pair? datum)
        (cdr datum)
        (error "Bad datum" datum)
    )
)

(define (rectangular? datum)
    (eq? (type-tag datum) 'rectangular)
)

(define (polar? datum)
    (eq? (type-tag datum) 'polar)
)

#| Modify the two representation and selectors |#

(define (real-part-rectangular z)
    (car z)
)

(define (imag-part-rectangular z)
    (cdr z)
)

(define (magnitude-rectangular z)
    (sqrt (+ (square (real-part-rectangular z)) (square (imag-part-rectangular z))))
)

(define (angle-rectangular z)
    (atan (imag-part-rectangular z) (real-part-rectangular))
)

(define (make-from-mag-ang-rectangular r theta)
    (attach-tag 'rectangular (cons (* r (cos theta)) (* r (sin theta))))
)

(define (make-from-real-imag-rectangular x y)
    (attach-tag 'rectangular (cons x y))
)

(define (magnitude-polar z)
    (cons z)
)

(define (angle-polar z)
    (cdr z)
)

(define (real-part-polar z)
    (* r (cos (magnitude-polar z)))
)

(define (imag-part-polar z)
    (* r (sin (magnitude-polar z)))
)

(define (make-from-mag-ang-polar r theta)
    (attach-tag 'polar (cons r theta))
)

(define (make-from-real-imag-polar x y)
    (attach-tag 'polar (cons (sqrt (+ (square x) (square y))) (atan y x)))
)

(define (real-part z)
    (cond ((rectangular? z) (real-part-rectangular (contents z)))
          ((polar? z) (real-part-polar (contents z)))
          (else (error "Wrong type" z))
    )
)

(define (imag-part z)
    (cond ((rectangular? z) (imag-part-rectangular (contents z)))
          ((polar? z) (imag-part-polar (contents z)))
          (else (error "Wrong type" z))
    )
)

(define (magnitude z)
    (cond ((rectangular? z) (magnitude-rectangular (contents z)))
          ((polar? z) (magnitude-polar (contents z)))
          (else (error "Wrong type" z))
    )
)

(define (angle z)
    (cond ((rectangular? z) (angle-rectangular (contents z)))
          ((polar? z) (angle-polar (contents z)))
          (else (error "Wrong type" z))
    )
)

(define (make-from-real-imag x y)
    (make-from-real-imag-rectangular x y)
)

(define (make-from-mag-ang r theta)
    (make-from-mag-ang-polar r theta)
)

(define (install-rectangular-package)
    (define (real-part z)
        (car z)
    )

    (define (imag-part z)
        (cdr z)
    )   

    (define (magnitude z)
        (sqrt (+ (square (real-part z)) (square (imag-part z))))
    )

    (define (angle z)
        (atan (imag-part z) (real-part z))
    )

    (define (make-from-real-imag x y)
        (cons x y)
    )

    (define (make-from-mag-ang r theta)
        (cons (* r (cos theta)) (* r (sin theta)))
    )

    (define (tag x)
        (attach-tag 'rectangular x)
    )

    (put 'real-part '(rectangular) real-part)
    (put 'imag-part '(rectagnular) imag-part)
    (put 'magnitude '(rectangular) magnitude)
    (put 'angle '(rectangular) angle)
    (put 'make-from-real-imag '(rectangular) (lambda (x y) (tag 'rectangular (make-from-real-imag x y))))
    (put 'make-from-mag-ang '(rectagnular) (lambda (r theta) (tag 'rectangular (make-from-mag-ang r theta))))
)

(define (install-polar-package)
    (define (magnitude z)
        (car z)
    )

    (define (angle z)
        (cdr z)
    )

    (define (real-part z)
        (* (magnitude z) (cos (angle z)))
    )

    (define (imag-part z)
        (* (magnitude z) (sin (angle z)))
    )

    (define (make-from-mag-ang r theta)
        (cons r theta)
    )

    (define (make-from-real-imag x y)
        (cons (sqrt (+ (square x) (square y))) (atan y x))
    )
    
    (define (tag x) (attach-tag 'polar x))

    (put 'magnitude '(polar) magnitude)
    (put 'angle '(polar) angle)
    (put 'real-part '(polar) real-part)
    (put 'imag-part '(polar) imag-part)
    (put 'make-from-mag-ang '(polar) (lambda (r theta) (tag (make-from-mag-ang r theta))))
    (put 'make-from-real-imag '(polar) (lambda (x y) (tag (make-from-real-imag x y))) )
)


(define (apply-generic op . args)
    (let ((type-tags) (mapper type-tag args))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (error "no Method for these types" (list op type-tags))
            )
        )

    )
)

(define (real-part z)
    (apply-generic 'real-part z)
)

(define (imag-part z)
    (apply-generic 'imag-part z)
)

(define (magnitude z)
    (apply-generic 'magnitude z)
)

(define (angle z)
    (apply-generic 'angle z)
)

(define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y)
)

(define (make-from-mag-ang r theta)
    ((get 'make-from-mag-ang 'polar) r theta)
)


(define (make-from-real-imag x y)
    (define (dispatch op)
        (cond ((eq? op 'real-part) x)
              ((eq? op 'imag-part) y)
              ((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
              ((eq? op 'angle) (atan y x))
              (else (error "Operation is not supported" op))
        )
    )
    dispatch
)

(define (apply-generic op arg) (arg op))
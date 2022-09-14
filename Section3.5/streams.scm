(define (stream-ref s n)
    (if (= n 0)
        (stream-car s)
        (stream-ref (stream-cdr s) (- n 1))
    )
)

#| (define (stream-map proc s)
    (if (stream-null? s)
        the-empty-stream
        (cons-stream (proc (stream-car s)) 
                     (stream-map proc (stream-cdr s))
        )
    )
) |#

(define (stream-for-each proc s)
    (if (stream-null? s)
        'done
        (begin (proc (stream-car s))
            (stream-for-each proc (stream-cdr s))
        )
    )
)

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))


(define (stream-enumerate-interval low high)
    (if (> low high)
        the-empty-stream
        (cons-stream 
            low
            (stream-enumerate-interval (+ low 1) high)
        )
    )
)

(define (stream-filter pred stream)
    (cond ((stream-null? stream) the-empty-stream)
          ((pred (stream-car stream)) 
           (cons-stream (stream-car stream) (stream-filter pred (stream-cdr stream))))
          (else (stream-filter pred (stream-cdr stream)))
    )
)       

; (define (force delayed-object)
;     (delayed-object)
; )


(define (memo-proc proc)
    (let ((already-run? #f) 
          (result #f))
        (lambda () (
            if (not already-run?)
               (begin (set! result (proc))
                      (set! already-run #t)
                      result)
               )
               result
        )
    )
)


; (define (delay exp)
;     (memo-proc (lambda () (exp)))
; )

(define (display-stream s)
    (stream-for-each display-line s)
)

(define (display-line x)
    (newline)
    (display x)
)


(define (integers-starting-from n)
    (cons-stream n (integers-starting-from (+ n 1)))
)

(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))

(define no-sevens
    (stream-filter (lambda (x) (not (divisible? x 7))) integers)
)

(define (fibgen a b)
    (cons-stream a (fibgen b (+ a b)))
)

(define fibs (fibgen 0 1))

(define (sieve stream)
    (cons-stream (stream-car stream)
                 (sieve (stream-filter 
                                (lambda (x) (not (divisible? x (stream-car stream))))
                                (stream-cdr stream)
                         )
                 )
    )
)

(define primes (sieve (integers-starting-from 2)))

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
    (stream-map + s1 s2)
)

(define integers (cons-stream 1 (add-streams ones integers)))

(define fibs
    (cons-stream 0
        (cons-stream 1
            (add-streams fibs (stream-cdr fibs))
        )
    )
)

(define (scale-stream stream factor)
    (stream-map (lambda (x) (* x factor)) stream)
)

(define double (cons-stream 1 (scale-stream double 2)))

(define primes 
    (cons-stream 2 (stream-filter prime? (integers-starting-from 3)))
)

(define (prime? n)
    (define (iter ps)
        (cond ((> (stream-car ps) (sqrt n)) #t)
              ((divisible? n (stream-car ps)) #f)
              (else (iter (stream-cdr ps)))
        )
    )
    (iter primes)
)

(define (sqrt-improve guess x)
    (average guess (/ x guess))
)

(define (sqrt-stream x)
    (define guesses
        (cons-stream 1.0 
                (stream-map (lambda (guess)
                                (sqrt-improve guess x)
                            )
                            guesses
                )
        )
    )
    guesses
)



(define (pi-summands n)
    (cons-stream (/ 1.0 n)
        (stream-map
            -
            (pi-summands (+ n 2))
        )
    )
)

(define pi-stream
    (scale-stream (partial-sums (pi-summands 1)) 4)
)

(define (euler-transform s)
    (let ((s0 (stream-ref s 0))
          (s1 (stream-ref s 1))
          (s2 (stream-ref s 2))
         )
         (cons-stream (- s2 (/ (square (- s2 s1)) (+ (- s0 s1) s2)))
            (euler-transform (stream-cdr s))
         )
    )
)

(define (make-tableau transform s)
    (cons-stream s
        (make-tableau transform (transform s))
    )
)

(define (accelerated-sequence transform s)
    (stream-map stream-car (make-tableau transform s))
)

(define (interleave s1 s2)
    (if (stream-null? s1)
        s2
        (cons-stream (stream-car s1)
                     (interleave s2 (stream-cdr s1))
        )
    )
)

(define (pairs s t)
    (cons-stream 
        (list (stream-car s) (stream-car t))
        (interleave
            (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
            (pairs (stream-cdr s) (stream-cdr t))
        )
    )
)

(define (integral integrand initial-value dt)
    (define int 
        (cons-stream
            initial-value
            (add-streams (scale-stream integrand dt)
                          int
            )
        )
    )
    int
)

(define (solve f y0 dt)
    (define y (integral dy y0 dt))
    (define dy (stream-map f y))
    y
)

#| Would not work  |#

(define (integral delayed-integrand initial-value dt)
    (define int
        (cons-stream initial-value
                     (let ((integrand (force delayed-integrand)))
                        (add-streams (scale-stream integrand dt)
                                      int
                        )
                      )
        )
    )
)

(define (solve f y0 dt)
    (define y (integral (delay dy) y0 dt))
    (define dy (stream-map f y))
    y
)

(define random-numbers
    (cons-stream
        random-init
        (stream-map rand-update random-numbers)
    )
)


(define (map-successive-pairs f s)
    (cons-stream
        (f (stream-car s) (stream-car (stream-cdr s)))
        (map-successive-pairs f (stream-cdr s))
    )
)

(define cesaro-stream
    (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
                          random-numbers
    )
)

(define (monte-carlo experiment-stream passed failed)
    (define (next passed failed)
        (cons-stream
            (/ passed (+ passed failed))
            (monte-carlo (stream-cdr experiment-stream) passed failed)
        )
    )
    (if (stream-car experiment-stream)
        (next (+ passed 1) failed)
        (next passed (+ failed 1))
    )
)

(define pi-stream
    (stream-map (lambda (p) (sqrt (/ 6 p)))
                (monte-carlo cesaro-stream 0 0)
    )
)
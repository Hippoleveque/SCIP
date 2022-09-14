#| Exercise 3.50 |#

(define (stream-map proc . argstreams)
    (if (stream-null? (car argstreams))
        the-empty-stream
        (cons-stream
            (proc (map stream-car argstreams))
            (apply stream-map (cons proc (map stream-cdr argstreams)))
        )
    )
)

#| Exercise 3.51 |#

(define (show x)
    (display-line x)
    x
)

#| 
(define x (stream-map show (stream-enumerate-interval 0 10)))

0 

(stream-ref x 5)

> The first one has been memoized
1 2 3 4 5

Value 5

(stream-ref x 7)
>  6 7
Value 7

|#

#| Exercise 3.52 |#

(define sum 0)

(define (accum x)
    (set! sum (+ x sum))
    sum
)

#| 

1. Value of sum

(define seq (stream-map accum (stream-enumerate-interval 1 20)))



> 1

(define y (stream-filter even? seq))

> 6

(define z (stream-filter (lambda (x) (= (remainder x 5)) 0) seq))

> 10

(stream-ref y 7)

(define seq (map accum (enumerate-interval 1 20)))

> 136

(display-stream z)

> 20 * 21 / 2 = 210


2. Yes, because when we evaluate 

(stream-ref y 7)


what happens is that the procedure 
(delay (stream-map accum (stream-cdr seq)))
is memoized at the different steps 

==> 


|#

#| Exercise 3.53 |#

(define s (cons-stream 1 (add-streams s s)))

#| 
The powers of 2
|#

#| Exercise 3.54 |#

(define (mul-streams s1 s2)
    (stream-map * s1 s2)
)

(define factorials 
    (cons-stream 
        1
        (mul-streams factorials (integers-starting-from 2))
    )
)

#| Exercise 3.55 |#

(define (partial-sums s)
    (cons-stream 
        (stream-car s)
        (add-streams (stream-cdr s) (partial-sums s))
    )
)

#| Exercise 3.56 |#

(define (merge s1 s2)
    (cond ((stream-null? s1) s2)
          ((stream-null? s2) s1)
          (else 
            (let ((s1car (stream-car s1))
                  (s2car (stream-car s2))
                )
                (cond ((< s1car s2car)

                       (cons-stream s1car (merge (stream-cdr s1) s2))
                      )
                      ((> s1car s2car) 
                       (cons-stream s2car (merge s1 (stream-cdr s2)))
                      )
                      (else 
                        (cons-stream s1car (merge (stream-cdr s1) 
                                                  (stream-cdr s2))
                        )
                      )
                )
            )
          )
    )
)

(define S (cons-stream 1 (merge (scale-stream S 2) 
                                (merge 
                                    (scale-stream S 3) 
                                    (scale-stream S 5)
                                )
                         )
          )
)


#| Exercise 3.57 |#

#| 
When we compute the nth Fibonacci number,
we compute n - 2 additions exactly

If delay was not memoized,
A(n) = A(n-1) + A(n-2)

|#

#| Exercise 3.58 |#

(define (expand num den radix)
    (cons-stream
        (quotient (* num radix) den)
        (expand (remainder (* num radix) den) den radix)
    )
)

#| 
The sequence will be as follows:
> n * r // d
> (n * r % d) * r // d
> (((n * r % d) * r ) % d) * r) // d

This is the list of the floating-point representation of 
num / den in base radix

|#

#| Exercise 3.59 |#

#| a. |#

(define (div-streams s1 s2)
    (stream-map / s1 s2)
)

(define (integrate-series seriesStream)
    (div-streams seriesStream integers)
)

#| b. |#

(define exp-series
    (cons-stream 1 (integrate-series exp-series))
)

(define (neg-stream s)
    (stream-map - s)
)

(define cosine-series
    (cons-stream 1 (neg-stream (integrate-series sine-series)))
)

(define sine-series
    (cons-stream 0 (integrate-series cosine-series))
)

#| Exercise 3.60 |#

(define (mul-series s1 s2)
    (cons-stream (* (stream-car s1) (stream-car s2))
            (add-streams (add-streams (scale-stream (stream-cdr s1) (stream-car s2)) 
                            (scale-stream (stream-cdr s2) (stream-car s1)))
                            (cons-stream 0 (mul-series (stream-cdr s1) (stream-cdr s2)))
                )
    )
)

#| Exercise 3.61 |#

(define (invert-unit-series s1)
    (cons-stream 1 
        (neg-stream (mul-series (stream-cdr s1) (invert-unit-series s1)))
    )
)

#| Exercise 3.62 |#

(define (div-series s1 s2)
    (if (= 0 (stream-car s2))
        (error "Can't divide by a series starting with 0")
        (mul-series s1 (invert-unit-series (scale-stream s2 (/ 1 (stream-car s2)))))
    )
)

#| Exercise 3.63 |#

#| original definition |#

(define (sqrt-improve guess x)
    (average guess (/ x guess))
)

(define (sqrt-stream x)
    (define guesses 
        (cons-stream 1.0
            (stream-map (lambda (guess) (sqrt-improve guess x))
                        guesses
            )
        )
    )
    guesses
)

#| Bad definition |#

(define (sqrt-stream x)
    (cons-stream 1.0
        (stream-map (lambda (guess) (sqrt-improve guess x)) 
                    (sqrt-stream x)
        )
    )
)

#| 
In the first definition, 
the cycle is as follows:
1. define guess 
2. Evaluate stream-cdr of guess
3. Enter stream-map and compute  the first value, built from the stream-car of guesses
4. At this point, stream-car guess is the first value of the stream map, and
the stream-cdr is the stream-map of (the stream-cdr of itself, and then when 
we re-evaluate the stream-cdr of itself, we can leverage memoization)

|#


#| 
In the second definition,
the cycle is as follows:
1. return a stream with 1.0 as its stream-car and stream-map of (sqrt-stream x) as 
its stream-cdr
2. When evaluation of stream-cdr of the result : 
We compute (sqrt-stream x), thus creating another stream with first value 1.0
We compute the second value of the result from this new stream and at the end the 
result as stream-car being the second value and stream-cdr being 
stream map on the (stream-cdr) of the new-stream

3. When we evaluate stream-cdr again:
We evaluate the stream-map of the stream-cdr of the new-stream
We begin by evaluating stream-cdr of the new-stream, thus evaluating
another time sqrt-stream and creating a third stream and doing the first steps again

We can quickly compute the complexity of the second stream 

Step (n) = Sum[k, 1, n - 1] (T(k)) + 1

T(n) = Sum[k, 1, n] (Step(k))

= Sum[k 1, n] (Sum[i, 1, n - 1] (T(k)) + 1)

T(1) = 1
T(2) = T(1) + T(1) + 1 = 3
T(3) = T(2) + T(1) + T(2) + 1 = 8
T(4) = T(3) + T(1) + T(2) + T(3) + 1 = 8 + 8 + 3 + 1 + 1 = 21
T(5)  = T(4) + (T1) + T(2) + T(3) + T(4) + 1 = 21 * 2 + 8 + 3 + 1 + 1 = 55


T(1) + T(2) + T(3) = T(4) - T(3) -  1

T(n) = 3 * T(n-1) - T(n - 2) - 1

T(n) = 3 * (3 * T(n - 2) - T(n - 3) - 1) - T(n - 2) - 1
     = 9 * T(n - 2) - 3 T(n - 3) - 3 - T(n - 2) - 1

Ok

If we had not implemented delay with memoization, we would not have
gain any complexity in time but we would have gain complexity in space
anyway 

|#

#|  Exercice 3.64 |#

(define (stream-limit s tol)
    (let ((curr (stream-car s))
          (next (stream-car (stream-cdr s)))
         )
         (if (> tol (abs (- curr next)))
             next
             (stream-limit (stream-cdr s) tol)
         )
    )
)

(define (sqrt x tolerance)
    (stream-limit (sqrt-stream x) tolerance)
)

#|  Exercice 3.65 |#

(define (log2-summands n )
    (cons-stream (/ 1.0 n)
        (stream-map - 
            (log2-summands (+ n 1))
        )
    )
)

(define log2-stream 
    (partial-sums (log2-summands 1))
)

#|  Exercice 3.66 |#

#| 
With the interleave procedure, one of two 
pairs will be selected among the first stream,
so it will take approx 200 pairs to generate 1, 100
Recursively, about 400 pairs to get 2,100; 
800 pairs to get 3, 100 and so on

To reach 99, 100, we'll need ~ 100 * 2 ** 99
To reach 100, 100, we'll need ~ 100 * 2 ** 100

|#

#|  Exercice 3.67 |#


(define (interleave3 s1 s2 s3)
    (cond ((stream-null? s1)
            (interleave s2 s3)
          )
          ((stream-null? s2)
           s3
          )
          (else (cons-stream
                    (stream-car s1)
                    (interleave3 s2 s3 (stream-cdr s1))
            ))
    )
)

(define (all-pairs s t)
    (cons-stream
        (list (stream-car s) (stream-car t))
        (interleave3
            (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
            (stream-map (lambda (x) (list x (stream-car s))) (stream-cdr t))
            (all-pairs (stream-cdr s) (stream-cdr s))
        )
    )
)

#|  Exercice 3.68 |#

(define (pairs s t)
    (interleave 
        (stream-map (lambda (x) (stream-car s) x) (stream-cdr t))
        (pairs (stream-cdr s) (stream-cdr t))
    )
)

#| 
As we use application order evaluation,
The interpreter will recursively evaluate (pairs (stream-cdr s) (stream-cdr t))
in an infinite loop
|#

#|  Exercice 3.69 |#

(define (triplets s t u)
    (cons-stream 
        (list (stream-car s) (stream-car t) (stream-car u))
        (interleave
            (stream-cdr (stream-map (lambda (x) (cons (stream-car s) x)) (pairs t u)))
            (triplets (stream-cdr s) (stream-cdr t) (stream-cdr u))
        )
    )
)

(define triples 
    (triplets integers integers integers)
)

(define pyth-triples
    (stream-filter 
        (lambda (x) (= (square (caddr x)) (+ (square (car x)) (square (cadr x)))))
        triples
    )
)

#|  Exercice 3.70 |#

(define (merge-weighted s1 s2 weight)
    (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else 
            (let ((s1car (stream-car s1))
                (s2car (stream-car s2))
                )
                (let ((w1 (weight s1car))
                      (w2 (weight s2car))
                      )
                    (cond ((< w1 w2)
                        (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight))
                        )
                        ((> w1 w2) 
                        (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight))
                        )
                        (else 
                            (cons-stream s1car (merge-weighted (stream-cdr s1) 
                                                    s2 weight)
                            )
                        )
                    )
                )
            )
        )
    )
)


(define (weighted-pairs s t weight)
    (cons-stream 
        (list (stream-car s) (stream-car t))
        (merge-weighted
            (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
            (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
            weight
        )
    )
)

#| a. |#

(define weighted-int-pairs
    (weighted-pairs integers integers (lambda (x) (+ (car x) (cadr x))))
)

#| b. |#

(define (not-divisible x num)
    (not (divisible? x num))
)

(define the-sum
    (let ((not-divisible-sum
          (stream-filter 
            (lambda (x)
                (and (not-divisible x 2)
                     (not-divisible x 3)
                     (not-divisible x 5)
                )
            )
            integers
          )
        ))
        (weighted-pairs not-divisible-sum
                        not-divisible-sum
                        (lambda (x)
                            (+ (* 2 (car x))
                               (* 3 (cadr x))
                               (* 5 (car x) (cadr x))
                            )
                        )
        )
    )
)

#|  Exercice 3.71 |#

(define cube-sum-weighted-integers-pairs
        (weighted-pairs
            integers
            integers
            (lambda (x) (+ (cube (car x)) (cube (cadr x))))
        )
)

(define (find-duplicate-weight s weight)
    (define (iter curr-s last-weight)
        (let ((curr-weight (weight (stream-car curr-s))))
            (if (= curr-weight last-weight)
                (cons-stream curr-weight
                            (iter (stream-cdr curr-s) 0)
                )
                (iter (stream-cdr curr-s) (weight (stream-car curr-s)))
            )
        )

    )
    (iter s 0)
)

(define ramanujan-nums
    (find-duplicate-weight cube-sum-weighted-integers-pairs
                           (lambda (x) (+ (cube (car x)) (cube (cadr x))))
    )
)

#|  Exercice 3.72 |#

(define square-sum-weighted-integers-pairs
    (weighted-pairs
        integers
        integers
        (lambda (x) (+ (square (car x)) (square (cadr x))))
    )
)

(define (find-triplicate-weight s weight)
    (define (iter curr-s)
        (let ((curr-weight (weight (stream-car curr-s)))
              (next-weight (weight (stream-ref curr-s 1)))
             )
             (if (= curr-weight next-weight)
                (let ((next-2-weight (weight (stream-ref curr-s 2))))
                     (if (= curr-weight next-2-weight)
                         (cons-stream
                            (cons (stream-car curr-s) curr-weight)
                            (cons-stream
                                (cons (stream-ref curr-s 1) curr-weight)
                                (cons-stream
                                    (cons (stream-ref curr-s 2) curr-weight)
                                    (iter (stream-cdr (stream-cdr (stream-cdr curr-s))))
                                )
                            )
                         )
                     )
                     (iter (stream-cdr (stream-cdr curr-s)))
                )
                (iter (stream-cdr curr-s))
             )
        )
    )
    (iter s)
)

(define desired-sum
       (find-triplicate-weight square-sum-weighted-integers-pairs
                              (lambda (x) (+ (square (car x)) (square (cadr x))))
       ) 
)

#| !!! Not good, skipping |#

#|  Exercice 3.73 |#

(define (RC R C dt)
    (define (proc si v0)
        (add-streams
            (integral (scale-stream si (/ 1 C)) v0 dt)
            (scale-stream si R)
        )
    )
    proc
)

#|  Exercice 3.74 |#

(define zero-crossings
    (stream-map sign-change-detector sense-data 
                                     (cons-stream
                                        0
                                        (stream-cdr sense-data)
                                     )
    )
)

#|  Exercice 3.75 |#

#| Louis' program |#

(define (make-zero-crossings input-stream last-value)
    (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
        (cons-stream (sign-change-detector avpt last-value)
                     (make-zero-crossings (stream-cdr input-stream)
                                          avpt
                    )
        )
    )
)

#| Correction |#

(define (make-zero-crossings input-stream last-value last-avpt)
    (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
        (cons-stream (sign-change-detector avpt last-avpt)
                    (make-zero-crossings (stream-cdr input-stream)
                                        (stream-car input-stream)
                                        avpt
                    )
        )
    )
)

#|  Exercice 3.76 |#

(define (smooth input-stream)
    (define (iter s last-value)
        (cons-stream (/ (+ (stream-car s) last-value) 2)
                     (iter (stream-cdr s) (stream-car s))
        )
    )
    (iter input-stream 0)
)

(define (smooth input-stream)
    (stream-map (lambda (x y) (/ (+ x y) 2)) input-stream (stream-cdr input-stream))
)

(define zero-crossings
    (let ((smoothed-data))
        (stream-map sign-change-detector smoothed-data 
            (cons-stream
                0
                (stream-cdr smoothed-data)
            )
        )
    )
)

#|  Exercice 3.77 |#

#| Original integral iterative style |#

(define (integral integrand initial-value dt)
    (cons-stream initial-value
        (if (stream-null? integrand)
            '()
            (integral (stream-cdr integrand)
                      (+ (* (stream-car integrand) dt) initial-value)
                      dt
            )
        )
    )
)

#| Modified integral iterative style |#

(define (integral delayed-integrand initial-value dt)
    (cons-stream initial-value
        (let ((integrand (force delayed-integrand)))
            (if (stream-null? integrand)
                '()
                (integral (stream-cdr integrand)
                          (+ (* (stream-car integrand) dt) 
                             initial-value)
                          dt
                )
            )
        )
    )
)   


#|  Exercice 3.78 |#

(define (solve-2nd a b y0 dy0 dt)
    (define y (integral (delay dy) y0 dt))
    (define dy (integral (delay ddy) dy0 dt))
    (define ddy (add-streams (scale-stream y b)
                             (scale-stream dy a)
    ))
    y
) 

#|  Exercice 3.79 |#

(define (solve-2nd f y0 dy0 dt)
    (define y (integral (delay dy) y0 dt))
    (define dy (integral (delay ddy) dy0 dt))
    (define ddy (stream-map f dy y))
    y
)

#|  Exercice 3.80 |#

(define (RLC R L C dt)
    (define (proc vc0 il0)
        (let ((vc (solve-2nd (lambda (dy y) (- (* R dy) (* (/ 1 (* L C)) y)))
                            vc0
                              (- (/ C il0))
                  )
               ))
              (let ((il (scale-stream (integral (delay vc) vc0 dt) C)))
                   (cons vc il)
             )
        )
    )
)

#|  Exercice 3.81 |#

(define (random-stream input-stream)
    (define random-stream
        (cons-stream
            random-init
            (stream-map (lambda (x input)
                    (if (eq? (car (stream-car input)) "generate")
                        (rand-update x)
                        (cdr (stream-car input))
                    )
                )
                random-stream 
                input-stream
            )
        )
    )
    random-stream
)


#|  Exercice 3.82 |#

(define (monte-carlo-stream-integral predicate x1 x2 y1 y2)
    (define (exp-stream)
        (cons-stream
            (predicate (cons (random-in-range x1 x2) (random-in-range y1 y2)))
            (exp-stream)
        )
    )
    (define monte-carlo-stream
        (scale-stream (monte-carlo exp-stream 0 0) (* (- x2 x1) (- y2 y1)))
    )
)
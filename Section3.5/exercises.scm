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
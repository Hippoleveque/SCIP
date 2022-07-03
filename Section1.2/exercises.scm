#| Exercice 1.9 |#

#| 
    1. The first procedure clearly generates a recursive process as inc is called 
    over and over again itself (a times exactly) until we reach the base case.

    2. The second procedure clearly generates an iterative process as we only keep
    track of the two arguments which are incremented and decremented a times.
|#

#| Exercice 1.10 |#

#| 
    1. f(n) = 2 * n

    2. g(n) = 2^n

    3. h(n) = 2^(2^n)

|#


#| Exercice 1.11 |#

(define (f-recursive n)
    (if (< n 3) n (+ (f-recursive (- n 1)) (* 2 (f-recursive (- n 2))) (* 3 (f-recursive (- n 3)))))
)


(define (f-iterative n)
    (define (iter a b c count)
        (if (< n count) c (iter b c (+ c (* 2 b) (* 3 a)) (+ count 1)))
    )
    (if (< n 3) n (iter 0 1 2 3))
)   


#| Exercice 1.12 |#

(define (pascal i j)
    (if (or (= 1 i) (= 1 j)) 1 (+ (pascal (- i 1) j) (pascal i (- j 1))))
)

#| Exercice 1.14 |#

#| https://www.ysagade.nl/2015/04/12/sicp-change-growth/ |#


#| Exercice 1.16 |#

(define (fast-expt-iter a b n)
    (cond ((= 0 n) a)
          ((even? n) (fast-expt-iter a (square b) (/ n 2)))
          (else (fast-expt-iter (* a b) b (- n 1)))
    )
)

#| Exercice 1.17 |#

(define (mul a b)
    (if (= 0 b) 0 (+ a (mul a (- b 1))))
)

(define (double a)
    (+ a a)
)

(define (even? a)
    (= 0 (remainder a 2))
)

(define (halve a)
    (/ a 2)
)

(define (fast-mul a b)
    (cond ((= b 0) 0)
          ((even? b) (mul (double a) (halve b)))
          (else (+ a (mul a (- b 1))))
    )
)

#| Exercice 1.18 |#

(define (fast-mul-iter current a b)
    (cond ((= 0 b) (+ current a))
          ((even? b) (fast-mul-iter current (double a) (halve b)))
          (else (fast-mul-iter (+ current a) a (- b 1)))
    )
)

#| Exercice 1.18 |#

(define (fib n)
    (fib-iter 1 0 0 1 n)
)

(define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count) (fib-iter a b (+ (square p) (square q)) (+ (square q) (* 2 p q)) (/ count 2)))
          (else (fib-iter (+ (* b q) (* a q) (* a p)) (+ (* b p) (* a q)) p q (- count 1)))
    )
)

#| Exercice 1.22 |#

(load "/Users/hippolyteleveque/Documents/learning_projects/computer_science/structure_interpretation/SCIP/Section1.2/primality.scm")

(define (start-prime-test n start-time)
    (if (is-prime? n) 
        (report-prime (- (runtime) start-time))
    )
)

(define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time)
)

(define (timed-prime-test n)
    (newline)
    (display n)
    (start-prime-test n (runtime))
)

(define (search-for-prime start end)
    (define (iter current)
        (if (> end current) (and 
            (timed-prime-test current)
            (iter (+ current 1))
        ) )
    )
    (iter start)
)

#| Exercice 1.23 |#

(load "/Users/hippolyteleveque/Documents/learning_projects/computer_science/structure_interpretation/SCIP/Section1.2/primality.scm")

(define (next n)
    (if (= n 2) 3 (+ n 2))
)

(define (smallest-divisor n)
    (define (iter candidate)
        (cond ((> (square candidate) n) n)
            ((= 0 (remainder n candidate)) candidate)
            (else (iter (next candidate)))
        )
    )
    (iter 2)
)

#| About 1.5 increase in performance because of the extra if step in next |#

#| Exercice 1.24 |#

(load "/Users/hippolyteleveque/Documents/learning_projects/computer_science/structure_interpretation/SCIP/Section1.2/primality.scm")

(define (start-prime-test n start-time)
    (if (fast-prime n  3) 
        (report-prime (- (runtime) start-time))
    )
)

#| It should be about as twice (supported by other's computations) |#

#| Exercice 1.25 |#

#| We are computing a lot of intermediary results instead of having to 
square and compute remainders for very large numbers  |#

#| Exercice 1.26 |#

#| By writing the procedure like this, at each step, we double the number of 
operations that need to be performed because we do the expmod procedure twice.
As a result, we need to perform 2^log2(n) = n procedures which would result in a
O(n) |#

#| Exercice 1.27 |#

(load "/Users/hippolyteleveque/Documents/learning_projects/computer_science/structure_interpretation/SCIP/Section1.2/primality.scm")

(fast-prime 561 10)
(fast-prime 1105 10)
(fast-prime 1729 10)
(fast-prime 2465 10)
(fast-prime 2821 10)
(fast-prime 6601 10)

#| Exercice 1.28 |#

(load "/Users/hippolyteleveque/Documents/learning_projects/computer_science/structure_interpretation/SCIP/Section1.2/primality.scm")


(define (check-nontrivial-sqrt1 x square m) 
(if (and (= square 1) 
     (not (= x 1)) 
     (not (= x (- m 1)))) 
0 
square)) 

(define (square-with-check x m)
    (check-nontrivial-sqrt1 x (remainder (square x) m) m)
)

(define (expmod-miller-rabin base exp m)
    (cond ((= 0 exp) 1)
        ((even? exp) (square-with-check (expmod base (/ exp 2) m) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))
    )
)

(define (miller-rabin-test n)
    (define (try a)
        (< 0 (expmod-miller-rabin a (- n 1) n))
    )
    (try (+ (random (- n 1)) 1))
)

(define (fast-prime n times)
    (cond ((= 0 times) #t)
        ((miller-rabin-test n) (fast-prime n (- times 1)))
        (else #f)
    )
)


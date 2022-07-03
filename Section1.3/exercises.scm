#| Exercice 1.29 |#

(load "/Users/hippolyteleveque/Documents/learning_projects/computer_science/structure_interpretation/SCIP/Section1.3/sum.scm")

(define (simpson-integral f a b n)
    (define (simpson-term x) 
        (define (next index)
            (cond ((or (= 0 index)(= n index)) (f x))
                  ((= 0 (remainder index 2)) (* 2 (f x)))
                  (else (* 4 (f x)))
            )
        )
        (next (/ (- x a)  (/ (- b a) n)))
    )
    (define (simpson-next x) (+ x (/ (- b a) n)))
    (* (/ (/ (- b a) n) 3)(sum simpson-term a simpson-next b))
)

(define (simpson-integral f a b n)
    (define h (/ (- b a) n))
    (define (y k) (f (+ a (* k h))))
    (define (term k)
        (cond ((or (= 0 k) (= n k)) (y k))
              ((= 0 (remainder k 2)) (* 2 (y k)))
              (else (* 4 (y x)))
        )
    )
    (define (inc k) (+ k 1))
    (* (/ h 3) (sum term 0 inc n))
)


#| Exercice 1.30 |#

(define (sum term a next b)
    (define (iter current accumulator)
        (if (> current b ) accumulator
            (iter (next current) (+ (term current) accumulator))
        )
    )
    (iter a 0)
)

#| Exercice 1.31 |#

(define (square x)
    (* x x)
)

(define (inc-two a)
    (+ a 2)
)


(define (recursive-product term a next b)
    (if (> a b) 1
        (* (term a) (recursive-product term (next a) next b))
    )
)

(define (product term a next b)
    (define (iter current accumulator)
        (if (> current b) accumulator
            (iter (next current) (* (term current ) accumulator))
        )
    )
    (iter a 1)
)



(* (/ (/  (recursive-product square 2.0 inc-two 100.0 ) (recursive-product square 3.0 inc-two 101.0 )) 2.0) 101.0)

(* (/ (/  (product square 2.0 inc-two 100.0 ) (product square 3.0 inc-two 101.0 )) 2.0) 101.0)


#| Exercice 1.32 |#

(define (recursive-accumulate combine null-value term a next b)
    (if (> a b) 
        null-value
        (combine (term a) (recursive-accumulate combine null-value term (next a) next b))
    )
)


(define (accumulate combine null-value term a next b)
    (define (iter current accumulator)
        (if (> current b) 
            accumulator
            (iter (next current) (combine (term current) accumulator))
        )
    )
    (iter a null-value)
)

(define (inc a)
    (+ a 1)
)

(define (identity a)
    (* 1 a)
)

(recursive-accumulate + 0 identity 0 inc 10)
(accumulate + 0 identity 0 inc 10)

(define (sum term a next b)
    (accumulate + 0 term a next b)
)

(define (product term a next b)
    (accumulate * 1 term a next b)
)

#| Exercice 1.33 |#

(define (filtered-accumulate combine null-value term a next b filter)
    (define (iter current accumulator)
        (cond ((> current b) accumulator)
              ((filter current) (iter (next current) (combine (term current) accumulator)))
              (else (iter (next current) accumulator))
        )
    )
    (iter a null-value)
)

(define (square-prime a b)
    (filtered-accumulate + 0 square a inc b is-prime?)
)

(define (relative-prime-product n)
    (define (relative-prime? a)
        (= 1 (gcd a n))
    )
    (filtered-accumulate * 1 identity 1 inc (- n 1) relative-prime?)
)

#| Exercice 1.34 |#

(define (f g)
    (g 2)
)

#| Exercice 1.35 |#

#| 
The golden ratio is such that 1 + ratio = ratio^2.
As such, if we devide both side by ratio, we have 1 + 1 / ratio = ratio
so that it is indeed a fixed point of x => 1 + 1 / x
|#

(define golden-ratio (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

#| Exercice 1.36 |#

(define (fixed-point f initial-guess)
    (let ((next (f initial-guess)))
        (display "New guess: ")
        (display next)
        (newline)
        (if (> tolerance (abs (- next initial-guess)))
            next
            (fixed-point f next)
        )
    )
)

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
(fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 2.0)

#| Exercice 1.37 |#

#| Recursive version |#
(define (cont-fract n d k)
    (define (iter current)
        (if (= current k)
            ( / (n current) (d current))
            (/ (n current) (+ (d current) (iter (+ current 1))))
        )   
    )
    (iter 1)
)

#| Iterative version |#
(define (cont-fract n d k)
    (define (iter current count)
        (if (= count 0)
            current
            (iter (/ (n count) (+ (d count) current)) (- count 1))
        )
    )
    (iter (/ (n k) (d k)) (- k 1))
)


#| Exercice 1.38 |#

(define (d k)
    (cond ((= 1 k) 1)
          ((= 0 (remainder (- k 2) 3)) (* 2.0 ( + 1.0 (/ (- k 2) 3))))
          (else 1)
    )
)

(define e (+ 2 (cont-fract (lambda (x) 1.0) d 25)))

#| Exercice 1.39 |#

(define (tan-cf x k)
    (define (d a)
        (+ 1 (* 2 (- a 1)))
    )
    (define (n a)
        (if (= 1 a)
            x
            (- (square x))
        )
    )
    (cont-fract n d k)
)

#| Exercice 1.40 |#

(define (cube x)
    (* x x x)
)

(define (cubic a b c)
    (lambda (x) (+ (cube x) (* a (square x)) (* b x) c))
)

(newton-method (cubic a b c) 1)


#| Exercice 1.41 |#

(define (double g)
    (lambda (x) (g (g x)))
)

(((double (double double)) inc) 5)

#| Exercice 1.42 |#

(define (compose f g) 
    (lambda (x) (f (g x)))
)

((compose square inc) 6)

#| Exercice 1.43 |#

#| Recursive implementation |#
(define (repeated g n)
    (if (= 1 n) 
        g
        (compose g (repeated g (- n 1)))
    )
)

#| iterative implementation |#

(define (repeated g n)
    (define (iter current count)
        (if (= 0 count) current
            (iter (compose g current) (- count 1))
        )
    )
    (iter identity n)
)

((repeated square 2) 5)


#| Exercice 1.44 |#

(define (smooth f)
    (let ((dx 0.0001))
         (let ((left-smooth (lambda (x) (f (- x dx))))
               (right-smooth (lambda (x) (f (+ x dx)))))
              (lambda (x) ( /(+ (left-smooth x) (f x) (right-smooth x)) 3))
         )
    )
)

(define (n-fold-smooth f n)
    ((repeated smooth n) f)
)

#| Exercice 1.45 |#

(define (nth-root x n)
    (fixed-point ((repeated average-damp (- n 1)) (lambda (y) (/ x (expt y (- n 1))))) 1.0)
)

#| Exercice 1.46 |#

#| recursive implementation |#
(define (iterative-improve good-guess? improve)
    (define (iter guess)
        (if (good-guess? guess)
            guess
            (iter (improve guess))
        )
    )
    (lambda (x) (iter x))
)


(define (average a b)
    (/ (+ a b) 2)
)


(define (sqrt x)
    (define (close-enough? guess)
        (> 0.01 (abs (- (square guess) x)))
    )
    (define (improve guess)
        (average guess (/ x guess))
    )
    ((iterative-improve close-enough? improve) 1.0)
)

(define (fixed-point f initial-guess)
    (define (close-enough? guess)
        (> 0.01 (abs (- (f guess) guess)))
    )
    (define (improve guess)
        (f guess)
    )
    ((iterative-improve close-enough? improve) initial-guess)
)
#|  Exercice 1.1 |#
; 1. 10
; 2. 12
; 3. 8
; 4. 3
; 5. 6
; 6. a
; 7. b
; 8. 19
; 9. #f
; 10. 4
; 11. 16
; 12. 6
; 13. 16



#| Exercice 1.2 |#

( / (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

#| Exercise 1.3 |#

(define (square x) (* x x))

(define (sum-two-squares x y)
    (+ (square x) (square y))
)

(define (>= x y) (not (< x y)))

(define (sum-two-larger-squares x y z)
    (sum-two-squares (max x y) (max (min x y) z))
)

#| Exercise 1.4 |#

(define (a-plus-abs-b a b) 
    ((if (> b 0) + -) a b)
)

#| Exercice 1.5 |#

#| 
    1. If the interpreter uses normal-order-evaluation, then the evaluation of the arguments 
    is delayed as much as possible, so that the test is first 'replaced' by its body, then the 
    'if' expression evaluate the condition with is true and therefore only evaluate the first 
    consequence expression with is 0 so that (p) is never evaluated.

    2. If the interpreter uses application-order-evaluation, then the arguments are evaluated before
    the body of 'test' replaces it. As a result, 0 is first evaluated, then (p) is infinitely evaluated:
    the evaluation is caught in an infinite loop.
|#

#| Exercice 1.6 |#

#| 
    As the interpreter uses application-order evaluation, all arguments of the new-if expression
    will be evaluated before the expression itself is. As a result, we will be caught in an infinite
    loop as the interpreter will infinitely try to evaluate the arguments, of which is the expreesion
    itself.
|#

#| Exercice 1.7 |#

#| 
    1. For very small numbers, say e-20, the threshold is too high compared to the value we are 
    working with.

    2. For very large numbers, it is too low.

    Main reason, we are looking to approximate x and using x^2 - y^2 as a test. 
    Or x^2 - y^2 = (x - y) * (x + y) so this will be very quickly low for small numbers and 
    very quickly high for high numbers.
|#

(define (good-enough? prevGuess guess)
    (< (abs (/ (- prevGuess guess) prevGuess)) 0.001)
)

(define (sqrt-iter prevGuess guess x)
    (if (good-enough? prevGuess guess) guess (sqrt-iter guess (improve guess x) x))
)

(define (sqrt x)
    (sqrt-iter 0 1.0 x)
)

#| Exercice 1.7 |#

(define (square x)
    (* x x)
)

(define (cube x)
    (* x (square x))
)

(define (improve guess x)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3)
)

(define (crt-iter prevGuess guess x)
    (if (good-enough? prevGuess guess) guess (crt-iter guess (improve guess x) x))
)

(define (crt x)
    (crt-iter 1.0 2.0 x)
)
#| Exercice 2.1 |#

(define (make-rat a b)
    (let ((g (gcd a b)))
        (if (< 0 (* a b)) 
            (cons (/ (abs a) g) (/ (abs b) g))
            (cons (/ (- (abs a)) g) (/ (abs b) g))
        )
    )
)

#| Exercice 2.2 |#


(define (make-segment a b)
    (cons a b)
)

(define (start-segment s)
    (car s)
)

(define (end-segment s)
    (cdr s)
)

(define (make-point x y)
    (cons x y)
)

(define (x-point a)
    (car a)
)

(define (y-point a)
    (cdr a)
)

(define (print-point p)
    (newline)
    (display "(")
    (display (x-point p))
    (display ",")
    (display (y-point p))
    (display ")")
)

(define (midpoint-segment) s
    (make-point (average (x-point (start-segment s)) (x-point (end-segment s)))
                (average (y-point (start-segment s)) (y-point (end-segment s)))
    )
)

#| Exercice 2.3 |#

#| First implementation from two segments (that share a point + form a straight angle) |#

(define (make-rectangle s1 s2)
    (cons s1 s2)
)

(define (s-length s)
    (sqrt (+ (square (x-point s)) (square (y-point s))))
)

(define (len r)
    (s-length (car r))
)

(define (width r)
    (s-length (cdr r))
)

(define (perimeter r)
    (+ (* 2 (len r)) (* 2 (width r)))
)

(define (area r)
    (* (len r) (width r))
)

#| Second implementation from corners top-right bottom-left |#

(define (make-rectangle p1 p2)
    (cons p1 p2)
)

(define (len r)
    (s-length (make-segment p1 (make-point (x-point p2) (y-point p1))))
)

(define (width r)
    (s-length (make-segment p1 (make-point (x-point p1) (y-point p2))))
)

#| Exercice 2.4 |#

(define (cons x y)
    (lambda (m) (m x y))
)

(define (car z)
    (z (lambda (x y) (x)))
)

(define (cdr z)
    (z (lambda (x y) (y)))
)

#| Exercice 2.5 |#

(define (cons a b)
    (* (expt 2 a) (expt 3 b))
)

(define (car z)
    (define (reduce x)
        (if (= 0 (remainder x 3))
            (reduce (/ x 3))
            x
        )
    )
    (/ (log (reduce z)) (log 2))
)

(define (cdr z)
    (define (reduce x)
        (if (= 0 (remainder x 2))
            (reduce (/ x 2))
            x
        )
    )
    (/ (log (reduce z)) (log 3))
)

#| Exercice 2.6 |#

(define (zero) (lambda (f) (lambda (x) x)))

(define (add-1 n)
    (lambda (f) (lambda (x) (f ((n f) x))))
)

(define (one)
    (lambda (f) (lambda (x) (f x)))
)

(define (two)
    (lambda (f) (lambda (x) (f (f x))))
)

#| Exercice 2.7 |#

(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y)) (+ (upper-bound x) (upper-bound y)))
)

(define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
          (p2 (* (upper-bound x) (upper-bound x)))
          (p3 (* (upper-bound x) (lower-bound y)))
          (p4 (* (lower-bound x) (upper-bound y)))
          (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))
         )
    )
)

(define (div-interval x y)
    (mul-interval x (make-interval (/ 1.0 (upper-bound y)) (/ 1.0 (lower-bound y))))
)

#| Exercice 2.8 |#

(define (sub-interval x y)
    (add-interval x (make-interval (- (upper-bound y)) (- (lower-bound y))))
)

#| Exercice 2.9 |#

(define (wdith x)
    (/ (- (upper-bound x) (lower-bound x)) 2)
)

#| 
1. Width of sum / diff
lower-bound (sum) = lower-bound (x) + lower-bound (y)
upper-bound (sum) = upper-bound (x) + upper-bound (y)

width (sum) = (upper-bound (x) + upper-bound (y) - lower-bound (x) - lower-bound (y)) / 2
width (sum) = (upper-bound (x) - lower-bound (y)) / 2 + (upper-bound (y) - lower-bound (y)) / 2
width (sum) = width (x) + width (y)

Similarly

lower-bound (diff) = lower-bound (x) - upper-bound (y)
upper-bound (diff) = upper-bound (x) - lower-bound (y)

width (diff) = (upper-bound (x) - lower-bound (y)  - lower-bound (x) + upper-bound (y)) / 2
width (diff) = (upper-bound (x) - lower-bound (x) + upper-bound (y) - lower-bound (y)) / 2
width (diff) = width (x) + width (y)

2. Width of mul
Suppose x = [0, 1] y = [0, 1]
width (x) = width (y) = 0.5

[mul (xy)] = [0, 1]
width (mul) = width (x) = width (y)

Suppose x = [0, 1] y = [0, 2]
[mul (xy)] = [0, 2] ok it's not a function of this

|#


#| Exercice 2.1O |#

(define (div-interval x y)
    (if ((> 0 (* (lower-bound y) (upper-bound y))))
        (error "Division by interval spanning zero is not supported")
        (mul-interval x (make-interval (/ 1.0 (upper-bound y)) (/ 1.0 (lower-bound y))))
    )
)

#| Exercice 2.11 |#

(define (mul-interval x y)
    (let ((x1 (lower-bound x))
        (x2 (upper-bound x))
        (y1 (lower-bound y))
        (y2 (upper-bound y)))
        
        (cond ((and (< 0 x1) (< 0 x2) (< 0 y1) (< 0 y2))
                (make-interval (* x1 y1) (* x2 y2)))
            ((and (> 0 x1) (< 0 x2) (< 0 y1) (< 0 y2))
                (make-interval (* x1 y2) (* x2 y2)))
            ((and (> 0 x1) (> 0 x2) (< 0 y1) (< 0 y2))
                (make-interval (* x1 y2) (* x2 y1)))
            ((and (< 0 x1) (< 0 x2) (> 0 y1) (< 0 y2))
                (make-interval (* x2 y1) (* x2 y2)))
            ((and (< 0 x1) (< 0 x2) (> 0 y1) (> 0 y2))
                (make-interval (* x2 y1) (* x1 y2)))
            ((and (> 0 x1) (< 0 x2) (> 0 y1) (< 0 y2))
                (make-interval (min (* x2 y1) * x1 y2) (max (* x2 y2) (* x1 y1))))
            ((and (> 0 x1) (> 0 x2) (> 0 y1) (< 0 y2))
                (make-interval (* x1 y2) (* x1 y1)))
            ((and (> 0 x1) (< 0 x2) (> 0 y1) (> 0 y2))
                (make-interval (* x2 y1) (* x1 y1)))
            ((and (> 0 x1) (> 0 x2) (> 0 y1) (> 0 y2))
                (make-interval (* x2 y2) (* x1 y1)))
        )
    )
)


#| Exercice 2.12 |#

(define (make-center-width c w)
    (make-interval (- c w) (+ c w))
)

(define (center i)
    (/ (+ (upper-bound i) (lower-bound i)) 2.0)
)

(define (width i)
    (/ (- (upper-bound i) (lower-bound i)) 2.0)
)

(define (make-center-interval c p)
    (let ((x (* c (1 - (/ p 100.0))))
          (y (* c (1 + (/ p 100.0))))
         )
         (make-interval x y)
    )
)

(define (percent i)
    (let ((c (center i))
          (x (lower-bound i))
        )
        (* 100.0 (/ (- c x) c))
    )
)

#| Exercice 2.13 |#

#| 
Soit [x1, y1] = [(1 - p1)a, (1 + p1)a] 
et   [x2, y2] = [(1 - p2)b, (1 + p2)b]

mul (XY1, XY2) = [(1 - p1)(1 - p2)ab, (1 + p1)(1 + p2)ab]
               = [(1 - p1 - p2 + p1p2)ab, (1 + p1 + p2 + p1p2)ab]

For small percentage, we can say that p1p2 ~ 0
So that mul (XY1, XY2) = [(1 - (p1 + p2))ab, (1 + p1 + p2)ab]
So that the tolerance is p1 + p2

|#

#| Exercice 2.14 |#

#| 
Soit A = [x, y] 
1 / A  = [1 / y , 1 / x]  
A / A = A * 1 / A = [x / y, y / x]

Soit R1 = [x1, y1]
     R2 = [x2, y2]

R1 * R2 = [x1 * x2, y1 * y2]
R1 + R2 = [x1 + x2, y1 + y2]

R1 * R2 / (R1 + R2) = [(x1 * x2) / (y1 + y2), (y1 * y2) / (x1 + x2)]

1 / R1 = [1 / y1, 1 / x1]
1 / R2 = [1 / y2, 1 / x2]

1 / R1 + 1 / R2 = [1 / y1 + 1 / y2, 1 / x1 + 1 / x2]

1 / (1 / R1 + 1 / R2)  = [ 1 / ( 1 / x1 + 1 / x2), 1 / (1 / y1 + 1 / y2)]

|#

#| Exercice 2.15 |#

#| 
She is right because the tolerance will add up each time we do an operation 

|#
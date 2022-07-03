(define (abs a)
    (if (< a 0) (- a) a)
)


(define (my-search f neg-point pos-point tolerance)
        (let ((midpoint (/ (+ neg-point pos-point) 2)))
            (if (> tolerance (abs (- neg-point pos-point)))
                midpoint
                (let ((midpoint-val (f midpoint)))
                    (cond ((= 0 midpoint-val) midpoint)
                        ((> 0 midpoint-val) (search f midpoint pos-point tolerance))
                        (else (search f neg-point midpoint tolerance))
                    )
                )
            )
        )
)

(define (average a b)
    (/ (+ a b) 2)
)

(define (positive? a)
    (> a 0)
)

(define (negative? a)
    (< a 0)
)

(define (close-enough? a b tol)
    (> tol (abs (- a b)))
)

(define (search f neg-point pos-point)
    (let ((midpoint (average neg-point pos-point)))
        (if (close-enough? neg-point pos-point 0.01)
            midpoint
            (let ((test-value (f midpoint)))
                (cond ((positive? test-value) (search f neg-point midpoint))
                      ((negative? test-value) (search f midpoint pos-point))
                      (else midpoint)
                )
            )
        )
    )
)

(define (half-interval-method f a b)
    (let ((a-value (f a))
          (b-value (f b))
         )
        (cond ((and (positive? a-value) (negative? b-value)) 
              (search f b a))
              ((and (negative? a-value) (positive? b-value)) 
              (search f a b))
              (else (error "Values are not of opposite sign" a b))      
        )
    )
)

(define tolerance 0.0001)

(define (fixed-point f initial-guess)
    (let ((test-value (f initial-guess)))
         (if (> tolerance (abs (- test-value initial-guess)))
             test-value
             (fixed-point f test-value)
         )
    )
)

(fixed-point (lambda (x) (+ (cos x) (sin x))) 1.0)

(define (sqrt x)
    (fixed-point (lambda (y) (/ (+ y (/ x y)) 2)) 1.0)
)

(define (average-damp f)
    (lambda (x) (average x (f x)))
)

(define (sqrt x)
    (fixed-point (average-damp (lambda (y) (/ x y))) 1.0)
)   

(define (square x)
    (* x x)
)

(define (cube-root x)
    (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0)
) 

(define h 0.0000001)

(define (deriv g)
    (let ((h 0.000001))
        (lambda (x) (/ (- (g (+ x h)) (g x)) h))
    )
)

(define (newton-transform g)
    (lambda (x) (- x (/ (g x) ((deriv g) x))))
)

(define (newton-method g guess)
    (fixed-point (newton-transform g) guess)
)

(define (sqrt x)
    (newton-method (lambda (y) (- (square y) x)) 1.0)
)

(define (fixed-point-of-transform g transform guess)
    (fixed-point (transform g) guess)
)

(define (sqrt x)
    (fixed-point-of-transform (lambda (y) (/ x y)) average-damp 1.0)
)

(define (sqrt x)
    (fixed-point-of-transform (lambda (y) (- (square y) x)) newton-transform 1.0)
)


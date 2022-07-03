(define (sum-pi a b)
    (sum (lambda (x) (/ 1.0 (* x (+ x 2)))) 
         a 
         (lambda (x) (+ 4 x))
         b
    )
)

(define (integral f a b dx)
   ( * dx (sum (lambda (x) (f (+ x (/ dx 2.0))))
         a
         (lambda (x) (+ x dx))
         b
    ))
)

(integral (lambda (x) (* x x x)) 0 1 0.01)


(define (f x y)
    (let ((a (+ 1 x))
          (b (- 1 y))
         )
         (+ (* x (square a)) (* y b) (* a b))
    )
)
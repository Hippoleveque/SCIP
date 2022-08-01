#| Exercise 2.77 |#

#|  
 It did not work before because the apply-generic was
 looking for either a polar and rectangular tag and
 found the complex tag.
 Now it works because it first found the complex 
 tag but now get back to the previous tag which
 would be either rectangular or polar and it will
 apply the appropriate procedure.
|#

#| Exercise 2.78 |#

(define (attach-tag type-tag contents)
    (if (number? contents)
        contents
        (cons type-tag contents)
    )
)

(define (type-tag datum)
    (cond ((number? datum) 'scheme-number)
          ((pair? datum) (car datum))
          (error "Bad tagged datum" datum)
    )
)

(define (contents datum)
    (cond ((number? datum) datum)
          ((pair? datum) (cdr datum))
          (error "Bad datum" datum)
    )
)


#| Exercise 2.79 |#

(define (equ? x y) (apply-generic 'equ? x y))

(define (install-scheme-number-package)
    ; 
    (put 'equ? '(scheme-number scheme-number) =)
)


(define (install-rational-package)
    ; ...
    (define (equ? x y) (= (* (num x) (denom y)) (* (denom x) (num y))))

    ; ...
    (put 'equ? '(rational rational) equ?)
)


(define (install-polar-package)
    ; ...
    (define (equ? x y) (and (= (angle x) (angle y)) (= (magnitude x) (magnitude y))))
    ; ...
    (put 'equ '(polar polar) equ?)
)

(define (install-rectangular-package)
    ; ...
    (define (equ? x y) (and (= (real-part x) (real-part y)) (= (imag-part x) (imag-part y))))
    ; ...
    (put 'equ? '(rectangular rectangular) equ?)
)

(define (install-complex-package)
    (put 'equ? '(complex complex) equ?)
    ; ...
)


#| Exercise 2.80 |#

(define (zero? x) (apply-generic 'zero? x))

(define (install-scheme-number-package)
    ; ...
    (put 'zero? '(scheme-number) (lambda (x) (= 0 x)))
    ; ...
)

(define (install-rational-package)
    ; ...
    (define (zero? x) (= (num x) 0))
    ; ...

    (put 'zero? '(rational) zero?)
)  

(define (install-rectangular-package)
    ; ...
    (define (zero? x) (and (= 0 (real-part x)) (= 0 (imag-part x))))
    ; ...

    (put 'zero? '(rectangular) zero?)
)

(define (install-polar-package)
    ; ...
    (define (zero? x) (= 0 (magnitude x)))
    ; ...

    (put 'zero? '(polar) zero?)
)

(define (install-complex-package)
    
    ; ...
    (put 'zero '(complex) zero?)
    ; ...
)

#| Exercise 2.81 |#

#| a. |#

#|  We'll fall in a infinite loop because 
a. The apply-generic won't find the operation
b. It'll try and manage to convert 
c. It'll get back to step a. 
|# 

#| b. |#

#| No, it is working fine as is, at least what 
we could modify is not about coercion of 
same type arguments but rather skipping 
the alternative statement if the types are the same
|#

#| c. |#


(define (apply-generic op . args)
(let ((type-tags (mapper type-tag args)))
    (let ((proc (get op type-tags)))
        (if proc
            (apply proc (mapper contents args))
            (if (and (= 2 (length args) 
                     (not (eq? (car type-tags) (cadr type-tags)))))
                (let ((a1 (car args))
                      (a2 (cadr args))
                      (t1 (car type-tags))
                      (t2 (cadr type-tags))
                    )
                    (let ((t1->t2 (get-coercion t1 t2))
                          (t2->t1 (get coercion t2 t1))
                        )
                        (cond ((t1->t2) (apply-generic op (t1->t2 a1) a2))
                              ((t2->t1) (apply-generic op a1 (t2->t1 a2)))
                              (else (error "No methods for theses types" (list op type-tags)))
                    
                        )
                    )    
                )
                (error "No method for these types" (list op type-tags))
            )
        )
    )
  )
)


#| Exercise 2.82 |#

#| 
This strategy will not be sufficient in general because you could have a third type 
reachabe for both subtypes where the operation is supported, though direct conversion 
between either of the two type is not possible. 

A good strategy would be to build a graph in two steps : 
- Each node would correspond to a type 
- First step : 
    - Create edges corresponding to mixed-type operation that we want to do
    - Mark connected components 
- Second step:
    - Create edges corresponding to type coercion 
    - Find a connected component that is reachable from all types in args

If such a connected component exists, it means that from all types within args,
we can reach a type within the connected component in the sens of step 1

|#

(define (apply-generic op . args)
    (define (iter-coerce-and-apply op type coerced args)
        (cond ((null? args) (op (mapper contents coerced)))
              ((eq? type (type-tag (car args))) (iter-coerce-and-apply op type (cons (car args) coerced) (cdr args)))
              ((if (get-coercion op (type-tag (car args)) type)) 
                (iter-coerce-and-apply op type (cons ((get-coercion op (type-tag (car args)) type) (contents (car args))) (cdr args)))
              )
              (else #f)
        )       
    )
    (define (iter type-args args)
        (if (null? type-args) 
            (error "Impossible to perform" op)
            (let ((result (iter-coerce-and-apply op (car type-args) '() args)))
                (if result 
                    result
                    (iter (cdr type-args) args)
                )
            )
        )

    )
    (let ((type-tags (mapper type-tag args)))
        (let (proc (get op type-tags))
            (if proc
                (apply proc (mapper contents args))
                (iter (type-tags) args)
            )
        )
    )   
)

#| Exercise 2.83 |#


(define (raise-int-to-rational n)
    (make-rational n 1)
)

(define (raise-rational-to-real r)
    (make-scheme-number (* 1.0 (/ (num r) (denom r))))
)

(define (raise-real-to-complex x)
    (make-from-real-imag x 0)
)

#| installation |#

(define (raise x) (apply-generic 'raise x))

(define (install-scheme-number-package)
    ; ...
    (put 'raise '(scheme-number) (lambda (x) (make-from-real-imag x 0)))
    ; ...
)

(define (install-rational-package)
    ; ...
    (put 'raise '(rational) (lambda (x) ((make-scheme-number (* 1.0 (/ (num r) (denom r)))))))
)  


(put 'raise '(integer) (lambda (x) (make-rational n 1)))



#| Exercise 2.84 |#


(put-order 'integer 0)
(put-order 'rational 1)
(put-order 'scheme-number 2)
(put-order 'complex 3)

(define (apply-generic op . args)
    (let ((type-tags (mapper type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (mapper contents args))
                (if (= 2 (length args))
                    (let ((a1 (car args))
                        (a2 (cadr args))
                        (t1 (car type-tags))
                        (t2 (cadr type-tags))
                        )
                        (let ((order1 (get-order t1))
                              (order2 (get-order t2))
                              )
                            (cond ((< order1 order2) (apply-generic op (apply-generic 'raise a1) a2))
                                  ((> order1 order2) (apply-generic op a1 (apply-generic 'raise a2)))
                                  (else (error "No method for these types" (list op type-tags)))
                            )
                        )    
                    )
                    (error "No method for these types" (list op type-tags))
                )
            )
        )
    )
)


#| Exercise 2.85 |#

(define (project x) (apply-generic 'project x))

(define (install-scheme-number-package)

    (define (project x)
        (define (iter n d))
        (if (= (round n) n)
            (make-rational n d)
            (iter (* 10 n) (* 10 d))
        )
        (iter x 1)
    )
    ; ...
    (put 'project '(scheme-number) project)
    ; ...
)

(define (install-rational-package)
    ; ...

    (put 'project '(rational) (lambda (x) (round (num x) (denom x))))
)  


(define (install-complex-package)
    
    ; ...
    (put 'project '(complex) (lambda (x) (make-scheme-number (real-part x))))
    ; ...
)

(define (drop x)
    (cond ((integer? x) x)
          ((equ? (project x) (raise x)) (project x))
          (else x)
    )
)


#| Exercise 2.86 |#


(define (install-scheme-number-package)

    ; ...
    (put 'sin '(scheme-number) (lambda (x) (sin x)))
    (put 'cos '(scheme-number) (lambda (x) (cos x)))
    ; ...
)

(define (install-rational-package)
    ; ...

    (put 'sin '(rational) (lambda (x) (sin (/ (num x) (denom x)))))
    (put 'sin '(rational) (lambda (x) (cos (/ (num x) (denom x)))))
)  

(define (sine x)
    (apply-generic 'sin x)
)

(define (cosine x)
    (apply-generic 'cos x)
)


(define (install-rectangular-package)
    ; ...
    (define (magnitude x)
        (sqrt (drop (add (mul (real-part x) (real-part x)) (mul (imag-part x) (imag-part x)))))
    )

    (define (angle x)
        (arctan (drop (imag-part x)) (drop (real-part x)))
    )
    ; ...

    (put 'zero? '(rectangular) zero?)
)

(define (install-polar-package)
    ; ...
    (define (imag-part x)
        (mul (magnitude x) (sine (angle x)))
    )

    (define (real-part)
        (mul (magnitude x) (cosine (angle x)))
    )
    ; ...

    (put 'zero? '(polar) zero?)
)

#| We would also replace the + * - to the operation for complex |#

#| Exercise 2.87 |#

(define (install-poly-pakage)
; ...
    (define (zero? p)
        (empty-termlist? (term-list p))
    )
; ...
)

#| Exercise 2.88 |#

(define (install-poly-pakage)
; ...
    (define (sub-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (varibale p1) (sub-terms (term-list p1) (term-list p2)))
            (else (error "Cannot sub poly with different variables"))
        )
    )



    (define (sub-terms L1 L2)
        (add-terms L1 (neg L2))
    )

    (define (neg L)
        (if (empty-termlist? L)
            (the-empty-termlist)
            (adjoin-term (make-term (order (first-term L)) (- (coeff (first-term L))))
                         (neg (rest-terms L))
            )
        )
    )
; ...

    (put 'neg '(polynomial) (lambda (L) (tag (neg L))))
)



(define (install-scheme-number-package)
    ; ...
    (put 'neg '(scheme-number) (lambda (x) (- x)))
    ; ...
)

(define (install-rational-package)
    ; ...
    (define (neg x) (make-rational (- (num x)) (denom x)))
    ; ...

    (put 'neg '(rational) (lambda (x) (tag (neg x))))
)  

(define (install-rectangular-package)
    ; ...
    (define (neg x) (make-from-real-imag (- (real-part x)) (- (imag-part x))))
    ; ...

    (put 'neg-complex '(rectangular) (lambda (x) (tag (neg x))))
)

(define (install-polar-package)
    ; ...
    (define (neg x) (make-from-real-imag (- (real-part x)) (- (imag-part x))))
    ; ...

    (put 'neg-complex '(polar) (lambda (x) (tag (neg x))))
)

(define (neg-complex z)
    (apply-generic 'neg-complex z)
)

(define (install-complex-package)
    
    ; ...
    (put 'neg '(complex) (lambda (x) (tag (neg-complex x))))
    ; ...
)

(define (neg x)
    (apply-generic 'neg x)
)

#| Exercise 2.89 |#

(define (adjoin-term term term-list)
    (define (adjoin-helper coeff num-zeros term-list)
        (if (= 0 num-zeros)
            (cons term term-list)
            (adjoin-helper coeff (- num-zeros 1) (cons 0 term-list))
        )
    )
    (if (zero? (coeff term))
        term-list
        (adjoin-helper (coeff term) (- (order term) (length term-list)) term-list)
    )
)

(define (the-empty-termlist) '())
(define (first-term L) (make-term (- (length L) 1) (car L)))
(define (rest-terms L) (cdr L))
(define (empty-termlist? L) (null? L))

(define (make-term order coeff)
    (list order coeff)
)

#| Exercise 2.90 |#


(define (install-poly-sparse-termlist)
    
    (define (make-term order coeff) (list order coeff))

    (define (empty-termlist? L) (null? L))
    (define (the-empty-termlist) '())
    (define (first-term L) (car L))
    (define (rest-term L) (cdr L))
    (define (order t) (car t))
    (define (coeff t) (cadr t))
    (define (adjoin-term term term-list)
        (if (zero? (coeff term))
            term-list
            (cons term term-list)
        )
    )


    (define (tag x) (attach-tag 'poly-sparse x))
    (put 'make 'poly-sparse (lambda (termlist) (tag termlist)))
    (put 'first-term '(poly-sparse) first-term)
    (put 'order '(poly-sparse) order)
    (put 'coeff '(poly-sparse) coeff)
    (put 'empty-termlist? '(poly-sparse) empty-termlist?)
    (put 'make-term '(poly-sparse) make-term)
    (put 'rest-term '(poly-sparse) rest-term)
    (put 'adjoin-term adjoin-term)
)

(define (install-poly-dense-termlist)

    (define (make-term order coeff)
        (list order coeff)
    )

    (define (adjoin-term term term-list)
        (define (adjoin-helper coeff num-zeros term-list)
            (if (= 0 num-zeros)
                (cons term term-list)
                (adjoin-helper coeff (- num-zeros 1) (cons 0 term-list))
            )
        )
        (if (zero? (coeff term))
            term-list
            (adjoin-helper (coeff term) (- (order term) (length term-list)) term-list)
        )
    )

    (define (empty-termlist? L) (null? L))
    (define (the-empty-termlist) '())
    (define (first-term L)
        (make-term (- (length L) 1) (car L))
    )
    (define (rest-term L) (cdr L))
    (define (order t) (car t))
    (define (coeff t) (cadr t))

    (define (tag x) (attach-tag 'poly-dense x))
    (put 'make 'poly-dense (lambda (termlist) (tag termlist)))
    (put 'first-term '(poly-dense) first-term)
    (put 'order 'poly-dense order)
    (put 'coeff 'poly-dense coeff)
    (put 'empty-termlist? '(poly-dense) empty-termlist?)
    (put 'make-term 'poly-dense make-term)
    (put 'rest-term '(poly-dense) rest-term)
    (put 'adjoin-term 'poly-dense adjoin-term)
)

(define (adjoin-term term term-list)
    ((get 'adjoin-term (type-tag term-list)) term term-list)
)

(define (first-term term-list)
    (apply-generic 'first-term term-list)
)

(define (rest-term term-list)
    (apply-generic 'rest-term term-list)
)

(define (install-poly-package)

    (define (make-poly variable term-list)
        (cons variable term-list)
    )

    (define (variable p)
        (car p)
    )

    (define (term-list p)
        (cdr p)
    )

    (define (add-terms L1 L2)
        (cond ((null? L1) L2)
              ((null? L2) L1)
              (else 
                 (let ((t1 (first-term L1))
                       (t2 (first-term L2))
                      )
                      (let ((o1 (order t1))
                            (o2 (order t2))
                            (c1 (coeff t1))
                            (c2 (coeff t2))
                           )
                           (cond ((> o1 o2) (adjoin-term (make-term o1 c1)
                                            (add-terms (rest-terms L1) L2))
                                 ((< o1 o2) (adjoin-term (make-term o2 c2) (add-terms L1 (rest-terms L2)))))
                                 ((= o1 o2) (adjoin-term (make-term o1 (+ c1 c2)) (add-terms (rest-terms L1) (rest-terms L2))))
                           ) 
                     )
                 )
              )
        )
    )


    (define (mul-terms L1 L2)
        (if (empty-termlist? L1)
            (the-empty-termlist)
            (add-terms (mul-term-by-all-terms (first-term L1) L2) (mul-terms (rest-terms L1) L2))
        )
    )

    (define (mul-term-by-all-terms t L)
        (if (empty-termlist? L)
            (the-empty-termlist)
            (adjoin-term (make-term (+ (order t) (order (first-term L)))
                                    (* (coeff t) (coeff (first-term L))))
                        (mul-term-by-all-terms t (rest-terms L))
            )
        )
    )

    (define (sub-terms L1 L2)
        (add-terms L1 (neg L2))
    )

    (define (neg L)
        (if (empty-termlist? L)
            (the-empty-termlist)
            (adjoin-term (make-term (order (first-term L)) (- (coeff (first-term L))))
                        (neg (rest-terms L))
            )
        )
    )

    (define (variable p) (car p))
    (define (term-list p) (cdr p))

    (define (add-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1) 
                    (add-terms (term-list p1) 
                                (term-list p2)
                    )
            )
        (error "Can't add polys with different variables")
        )
    )

    (define (mul-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1)
                       (mul-terms (term-list p1)
                                  (term-list p2)
                       )
            )
            (error "Can't add polys with different variables")
        )
    )

    (define (sub-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1)
                       (sub-terms (term-list p1)
                                  (term-list p2)
                       )
            )
        )
    )

    (define (tag x) (attach-tag 'poly x))
    (put 'make 'poly (lambda (variable termlist) (tag (make-poly (variable termlist)))))
    (put 'add '(poly poly) (lambda (p1 p2) (tag (add-poly p1 p2))))
    (put 'mul '(poly poly) (lambda (p1 p2) (tag (mul-poly p1 p2))))
    (put 'sub '(poly poly) (lambda (p1 p2) (tag (sub-poly p1 p2))))
)

#| Exercise 2.91 |#

(define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1)))
             ((t2 (first-term L2)))
             (if (> (order t2) (order t1))
                 (list (the-empty-termlist) L1)
                 (let ((new-c (div (coeff t1) (coeff t2)))
                       (new-o (- (order t1) (order t2)))
                      )
                      (let ((rest-of-result (sub L1 (mul (adjoin-term (make-term new-o new-c) (the-empty-termlist)) L2 ))))
                          (let ((rec-result (div-terms rest-of-result L2)))
                            (list (adjoin-term (car rec-result)) (cadr rec-result))
                          )
                    
                      )
                 )
             )
        )
    )
)

#| Book |#
(define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1)))
            ((t2 (first-term L2)))
            (if (> (order t2) (order t1))
                (list (the-empty-termlist) L1)
                (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2)))
                    )
                    (let ((rest-of-result (div-terms (sub L1 (mul-term-by-all-terms (make-term new-o new-c) L2)) L2))))
                            (list (adjoin-term (make-term new-o new-c) (car rest-of-result)) (cadr rest-of-result))
                )
            )
        )
    )
)

#| Exercise 2.92 |#

#| Let's suppose we have a procedure gt that take two variable and say which one 
is the highest priority

This is a short tentative
|#

(define (install-poly-package)
    ; ...
    (define (add-poly p1 p2)
        (cond ((same-variable? p1 p2)
            (make-poly (variable p1) (add-terms (term-list p1) (term-list p2))))
            ((gt (variable p1) (variable p2))
                (add-poly p1 (switch-variable p2 (variable p1)))
            )
            ((gt (variable p2) (variable p1)))
        )
    )

    (define (switch-variable p var)
        (define (helper terms)
            (let ((t1 (first-term terms)))
                 (if (poly? (coeff t1))
                     (let ((tp1 (switch-variable t1 var)))
                         (add (mul-const (make-poly (variable p) (adjoin-term (make-term (order t1) 1))) tp1) (helper (rest-terms terms)))
                     )
                 )
                 (else (add-const (make-poly (variable p) (ajoin-term t1 (empty-termlist?))) (helper (rest-terms terms))))
            )
        )

        (if (= (variable p) var)
            p
            (make-poly var (helper (term-list p)))
        )
    )

    ; Could be representation specific but it would work in both our case

    (define (add-const const terms)
        (cond ((empty-termlist? terms) (adjoin-term const (the-empty-termlist)))
            ((empty-termlist? (rest-terms terms)) (add (first-term terms) const))
            (else (adjoin-term (first-term terms) (add-const const (rest-terms terms))))
        )
    )

    (define (mul-const const terms)
        (if (empty-termlist? terms) 
            (the-empty-termlist)
            (adjoin-term (mul const (first-term terms)) (mul-const const (rest-terms terms)))
        )
    )
    ; ... 
)


#| Exercise 2.93 |#

(define (install-rational-package)
    ; ...
    (define (make a b)
        (let ((g (gcd a b)))
            (tag (cons (div a g) (div b g)))
        )
    )
    (define (num x) (car x))
    (define (denom x) (cdr x))
    (define (add-rat x y)
        (make (add (mul (num x) (denom y)) (mul (num y) (denom x))) (mul (denom x) (denom y)))
    )
    (define (sub-rat x y)
        (make (sub (mul (num x) (denom y)) (mul (num y) (denom x))) (mul (denom x) (denom y)))
    )
    (define (mul-rat x y)
        (make (mul (num x) (num y)) (mul (denom x) (denom y)))
    )
    (define (div-rat x y)
        (make (mul (num x) (denom y)) (mul (denom x) (num y)))
    )
    
    (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))
    (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))
    (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))
    (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))
    (put 'make 'rational (lambda (a b) (tag (make a b))))
    'done
)

(define (install-poly-package)
    ; ...
    (define (gcd-terms L1 L2)
        (if (empty-termlist? L1) 
            L1
            (gcd-terms L2 (remainder-terms L1 L2))
        
        )
    )
)

#| Exercise 2.94 |#

(define (remainder-terms L1 L2)
     (cadr (div-terms (term-list L1) (term-list L2)))
)

(define (gcd-poly p1 p2)
    (if (same-variable? (variable p1)  (variable p2))
        (make-poly (variable p1) (gcd-terms (term-list p1) 
                                            (term-list p2)))
        (error ("The variable of the two polynoms must be the same"))
    )
)

#| Exercise 2.95 |#

#| Can't do |#

#| Exercise 2.96 |#

#| Won't do |#

#| Exercise 2.97 |#

#| Won't do |#
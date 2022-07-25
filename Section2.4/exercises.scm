#| Exercise 2.73 |#

(define (deriv expr var)
    (cond ((number? expr) 0)
          ((and (variable? expr) (same-variable? expr var)) 1)
          ((sum? expr) (make-sum (deriv (addend expr) var) (deriv (augend expr) var)))
          ((product? expr) (make-sum (make-product 
                                                    (multiplicand expr)
                                                    (deriv (multiplir expr) var)
                                     )
                                     (make-product 
                                                    (deriv (multiplicand expr) var)
                                                    (multiplier expr)
                                     )
                            )
          )
          (else (error "Malformed expression"))
    )
)

(define (deriv expr var)
    (cond ((number? expr) 0)
          ((and (variable? expr) (same-variable? expr var)) 1)
          (else ((get 'deriv (operator expr)) (operand expr) var))
    )
)

(define (operator expr)
    (car expr)
)

(define (operand expr)
    (cdr expr)
)

#| a. |#

#|  
Because they are primitive type that are not constructed with an operator
|#

#| b. |#

(define (install-deriv-package)
    (define (deriv-sum exp var)
        (make-sum (deriv (addend exp) var) (deriv (augend exp) var))
    )

    (define (deriv-prod exp var)
        (make-sum (make-product (deriv (multiplier exp) var) (multiplicand exp))
               (make-product (multiplier exp) (deriv (multiplicand exp) var))
        )
    )

    (put 'deriv 'sum deriv-sum)
    (put 'deriv 'prod deriv-prod)
)


#| c. |#

(define (install-deriv-package)
    (define (deriv-sum exp var)
        (make-sum (deriv (addend exp) var) (deriv (augend exp) var))
    )

    (define (deriv-prod exp var)
        (make-sum (make-product (deriv (multiplier exp) var) (multiplicand exp))
            (make-product (multiplier exp) (deriv (multiplicand exp) var))
        )
    )

    (define (deriv-exp exp var)
        (make-product (make-product (exponent exp) (deriv (base exp) var))
                      (make-exponentiation (base exp) (- (exponent exp) 1))
        )
    )

    (put 'deriv '+ deriv-sum)
    (put 'deriv '* deriv-prod)
    (put 'deriv '** deriv-exponent)
)


#| d. |#

#| 
Changes: 
- in deriv invert 'deriv and (operator expr)
- in install-deriv-package invert it on the put statements 

|#


#| Exercise 2.74 |#

#| a. |#

(define (get-record key file)
    ((get 'lookup (division file)) key (records file))
)

#| We need to have the division to which the file belong |#

#| b. |#

(define (get-salary key record)
    ((get 'salary (division record)) key (content record))
)

#| 
We don't care how the record is structured as long as we have
- a table where there is a salary function for each of 
- a way to access the division of the record
|#

#| c.  |#

(define (find-employee-record name files)
    (if (null? files) 
        #f
        (let ((find-record (get-record (car files)))
             )
            (if find-record
                find-record
                (find-employee-record name (cdr files))
            )
        )
    )
)

#| d.  |#

#| The only thing that need to change is the table where there is the lookup 
and get-salary implementations|#


#| Exercise 2.75 |#

(define (make-from-mag-ang r theta)
    (define (dispatch op)
        (cond ((eq? op 'magnitude) r)
              ((eq? op 'angle) theta) 
              ((eq? op 'real-part) (* r (cos theta)))
              ((eq? op 'imag-part) (* r (sin theta)))
              (else (error "Operation not supported" op))
        )
    )
    (dispatch)
)


#| Exercie 2.76 |#

#| 
1. For the strategy with generic operations with explicit dispatch, 
    a. When a new operation is added, you just need to add it and write it
    explictly for the different types
    b. When a new type is added, you need to go and modify each procedure
    to write it when the new type is added

2. For data-directed style 
    a. When a new operation is added, you add a new row in the table i.e 
    you define it for each type and put it into the table
    b. When a new type is added, you add a new column into the table and 
    add the corresponding implementation of each of existing operation for
    the new type in the table

3. For message-passing style
    a. When a new operation is added, you need to modify each of your type
    to implement the new operation there
    b. When a new type is added, you just need to implement directly within
    the different operations

I. If the system often add new types, it seems unwise to use 1. as you would
need to modify all procedures which is tedious and error-prone, going with 
either 2. or 3. seems much wiser

II. If the system often add new operation, it seems likely unwise to use 3. 
as you need to modify all your types whereas going with 1. or 2. seems fine.


|#
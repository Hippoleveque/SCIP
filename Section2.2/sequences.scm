#| Recursive process implementation |#

(define (count-leaves s)
    (cond ((null? s) 0)
          ((pair? (car s)) (+ 
                                (count-leaves (car s)) 
                                (count-leaves (cdr s))
                            ))
          (else (+ 1 (count-leaves (cdr s))))
    )
)

#| Iterative process implementation |#

(define (count-leaves s)
    (define (iter current answer)
        (cond ((null? current) answer)
              ((pair? (car current)) (iter (cdr current) (+ answer (iter (car current) 0))))
              (else (iter (cdr current) (+ 1 answer)))
        )
    )
    (iter s 0)
)


#| Simpler, book implementation |#

(define (count-leaves t)
    (cond ((null? t) 0)
          ((pair? t) (+ (count-leaves (car t)) (count-leaves (cdr t))))
          (else 1)
    )
)

#| Book implementation as an pseudo-iterative process |#

(define (count-leaves t)
    (define (iter current answer)
        (cond ((null? current) answer)
              ((pair? current) (iter (cdr current)  (+ answer (iter (car current) 0))))
              (else 1)
        )
    )
    (iter t 0)
)

(define (scale-tree t factor)
    (cond ((null? t) '())
          ((pair? t) (cons (scale-tree (car t) factor) (scale-tree (cdr t) factor)))
          (else (* t factor))
    )
)

(define (scale-tree t factor)
    (define (helper x)
        (cond ((null? x) '())
            ((pair? x) (scale-tree x factor))
            (else (* x factor))
        )
    )
    (mapper helper t)
)
 
(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 11)

(define (sum-odd-squares tree)
    (cond ((null? tree ) 0)
          ((pair? tree) (+ (sum-odd-squares (car tree)) (sum-odd-squares (cdr tree))))
          (else (if (odd? tree) (square tree) 0))
    )
)

(define (even-fibs n)
    (define (next k)
        (if (> k n)
            '()
            (let ((fibk (fib k)))
                (if (odd? fibk)
                    (next (+ k 1))
                    (cons fibk (next (+ k 1)))
                )
            )
        )
    )
    (next 0)
)


(define (filter predicate sequence)
    (cond ((null? sequence) '())
          ((predicate (car sequence)) (cons (car sequence) (filter predicate (cdr sequence))))
          (else (filter predicate (cdr sequence)))
    )
)

(define (accumulate op initial sequence)
    (if (null? sequence) 
        initial
        (op (car sequence) (accumulate op initial (cdr sequence)))
    )
)

(define (enumerate-interval low high )
    (if (> low high)
        '()
        (cons low (enumerate-interval (+ 1 low) high))
    )
)

(define (enumerate-tree tree)
    (define (iter current answer)
        (cond ((null? current) answer)
              ((pair? current) (iter (car current) (iter (cdr current) answer)))
              (else (cons current answer))
        )
    )
    (iter tree '())
)

(define (sum-odd-squares tree)
    (accumulate + 
                0
                (mapper square 
                        (filter odd?
                                (enumerate-tree tree)
                        )
                
                )
    )
)

(define (even-fibs n)
    (accumulate cons
                '()
                (filter even?
                        (mapper fib
                            (enumerate-interval 0 n)
                        )
                )
    )
)

(define (list-fib-squares n)
    (accumulate cons
                '()
                (mapper square
                        (mapper fib 
                                (enumerate-interval 0 n)
                        )
                )
    )
)

(define (product-of-squares-of-odd-elements seq)
        (accumulate *
                    1
                    (mapper square
                            (filter odd? 
                                    seq
                            )
                    )
        )
)

(define (salary-of-highest-paid-programmer records)
    (accumulate max
                0
                (mapper salary
                        (filter programmer?
                                records
                        )
                )
    )
)

(define (enumerate-ordered-pairs n)
    (accumulate append
                '()
                (mapper (lambda (i) (
                            mapper (lambda (j) (list j i))
                                    (enumerate-interval 1 (- i 1))       
                ))
                    (enumerate-interval 2 n)
                )
    )
)




(define (ordered-pairs-sum-prime n)
    (filter (lambda (x) (is-prime? (+ (car x) (car (cdr x)))))
            (enumerate-ordered-pairs n)
    )
)

(define (flatmap proc seq)
    (accumulate append
                '()
                (mapper proc seq)
    )
)

(define (prime-sum? pair)
    (is-prime? (+ (car pair) (cadr pair)))
)

(define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair)))
)

(define (prime-sum-pair n)
    (mapper make-pair-sum
            (filter prime-sum?
                    (flatmap 
                            (lambda (i) (mapper (
                                lambda(j) (list j i))
                                (enumerate-interval 1 (- i 1))
                            ))
                            (enumerate-interval 2 n)
                            
                    )
            )
    )
)


(define (remove-from-seq el seq)
    (cond ((null? seq) '())
          ((= (car seq) el)  (cdr seq))
          (else (cons (car seq) (remove-from-seq el (cdr seq))))

    )
)

(define (remove el seq)
    (filter (lambda (x) (not (= x el)))
        seq
    )
)

(define (all-permutations s)
    (if (null? s)
        (list '())
        (flatmap
                (lambda (x) (
                    mapper (lambda (p) (cons x p))
                           (all-permutations (remove x s))

                ))
                s
        )
    )
)
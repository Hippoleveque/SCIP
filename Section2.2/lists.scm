(define (list-ref alist n)
    (if (= 0 n) (car alist)
        (list-ref (cdr alist) (- n 1))
    )
)

(define (rec-length alist)
    (if (null? alist) 0
        (+ 1 (rec-length (cdr alist)))
    )
)

(define (rec-length alist)
    (define (iter count rest)
        (if (null? alist) count
            (iter (+ 1 count) (cdr rest))
        )
    )
)

(define (append list1 list2)
    (if (null? list1) 
        list2
        (cons (car list1) (append (cdr list1) list2))
    )
)

(define (scale-list alist factor)
    (if (null? alist)
        alist
        (cons (* (car alist) factor) (scale-list (cdr alist) factor))
    )
)

(define (mapper proc alist)
    (if (null? alist)
        alist
        (cons (proc (car alist)) (map proc (cdr alist)))
    )
)

(define (scale-list alist factor)
    (mapper (lambda (x) (* x factor)) alist)
)
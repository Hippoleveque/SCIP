#| Sets as unordered lists |#

(define (element-of-set? x s)
    (cond ((null? s) #f)
          ((equal? x (car s)) #t)
          (else (element-of-set? x (cdr s)))
    )
)

(define (ajoin-set x s)
    (if (element-of-set? x s) s (cons x s))
)

(define (intersection-set s1 s2)
    (cond ((null? s1) s2)
          ((element-of-set? (car s1) s2) (cons (car s1) (intersection-set (cdr s1) s2)))
          (else (intersection-set (cdr s1) s2))
    )
)

#| Sets as ordered lists |#

(define (element-of-set? x s)
    (cond ((null? s) #f)
          ((> (car s) x) #f)
          ((equal? x (car s)) #t)
          (else (element-of-set? x (cdr s)))
    )
)

(define (intersection-set s1 s2)
    (cond (or ((null? s1) (null? s2)) '())
          ((> (car s1) (car s2)) (intersection-set s1 (cdr s2)))
          ((< (car s1) (car s2)) (intersection-set (cdr s1) s2))
          ((equal? (car s1) (car s2)) (cons (car s1) (intersection-set (cdr s1) (cdr s2))))
    )
)

#| Sets as binary trees |#

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree)) 

(define (make-tree entry left right) (list entry left right))

(define (element-of-set? x set)
    (cond ((null? set) #f)
          ((= x (entry set)) #t)
          ((> x (entry set)) (element-of-set? x (right-branch set)))
          ((< x (entry set)) (element-of-set? x (left-branch set)))
    )
)

(define (adjoin-set x set)
    (cond ((null? set) (make-tree x '() '()))
          ((= x (entry set)) set)
          ((> x (entry set)) (list (entry set) (left-branch set) (adjoin-set x (right-branch set))))
          ((< x (entry set)) (list (entry set) (adjoin-set x (left-branch set)) (right-branch set)))
    )
)

(define (lookup-unordered given-key records)
    (cond ((null? records) #f)
          ((= given-key (key (car records))) (car records))
          (else (lookup-unordered given-key (cdr records)))
    )
)
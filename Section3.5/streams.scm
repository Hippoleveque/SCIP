(define (stream-ref s n)
    (if (= n 0)
        (car-stream s)
        (stream-ref (stream-cdr s) (- n 1))
    )
)

(define (stream-map proc s)
    (if (stream-null? s)
        the-empty-stream
        (cons-stream (proc (stream-car s)) 
                     (stream-map proc (stream-cdr s))
        )
    )
)

(define (stream-for-each proc s)
    (if (stream-null? s)
        'done
        (begin (proc (stream-car s))
            (stream-for-each proc (stream-cdr s))
        )
    )
)

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
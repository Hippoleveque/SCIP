#| Assuming that a procedure get-new-pair with unassigned cons and car exists |#

(define (cons x y)
    (let ((new (get-new-pair)))
        (set-car! new x)
        (set-cdr! new y)
        new
    )
)

#| Mutable data objects are just assignment, example with cons car cdr |#

(define (cons x y)
    (define (set-x v) (set! x v))
    (define (set-y v) (set! y v))
    (define (dispatch m)
        (cond ((eq? m 'car) x)
              ((eq? m 'cdr) y)
              ((eq? m 'set-car) set-x)
              ((eq? m 'set-cdr) set-y)
              (else (error "Operation not supported" m))
        )
    )
    dispatch
)

(define (car z) (z 'car))
(define (cdr z) (z 'cdr))

(define (set-car! z new-value) ((z 'set-car) new-value))
(define (set-cdr! z new-value) ((z 'set-cdr) new-value))


(define (make-queue)
    (cons '() '())
)

(define (front-ptr q) (car q))
(define (rear-ptr q) (cdr q))
(define (set-front-ptr! q item) (set-car! q item))
(define (set-rear-ptr! q item) (set-cdr! q item))

(define (empty-queue? q) (null? (front-ptr q)))

(define (front-queue q)
    (if (empty-queue? q)
        (error "The queue is empty" q)
        (car (front-ptr q))
    )
)

(define (insert-queue! q item)
    (let ((new (cons item '())))
        (if (empty-queue? q)
            (begin (set-front-ptr! q new) (set-rear-ptr! q new))
            (begin (set-cdr! (rear-ptr q) new) (set-rear-ptr! q new) q)
        )
    )
)

(define (delete-queue! q)
    (if (empty-queue? q)
        (error "The queue is empty" q)
        (set-front-ptr! q (cdr (front-ptr q)) q)
    )
)

#| Tables |#

(define (lookup key table)
    (let ((record (assoc key (cdr table))))
        (if record
            (cdr record)
            #f
        )
    )
)

(define (assoc key records)
    (cond ((null? records) #f)
          ((equal? (caar records) key) (car records))
          (else (assoc key (cdr records)))
    )
)

(define (insert! key value table)
    (let ((record (assoc key (cdr table))))
        (if record
            (set-cdr! record value)
            (let ((new (cons (cons key value) (cdr table))))
                (set-cdr! table new)
            )
        )
    )
)


(define (make-table)
    (list '*table*)
)

(define (lookup-2d key-1 key-2 table)
    (let ((subtable (assoc key-1 (cdr table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
                (if record
                    (cdr record)
                    #f
                )
            )
            #f
        )
    )
)

#| My implementation (to be tested) |#

(define (insert-2d! key-1 key-2 value table)
    (let ((subtable (assoc key-1 (cdr table))))
        (if subtable
            (insert! key-2 value subtable)
            (let ((new-el (cons (cons key-2 value) '())))
                (let ((new-subtable (cons (cons key-1 new-el) (cdr table))))
                    (set-cdr! table new-subtable)
                )
            )
        )
    )
)

#| Book implementation |#

(define (insert-2d! key-1 key-2 value table)
    (let ((subtable (assoc key-1 (cdr table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
                (if record
                    (set-cdr! record value)
                    (set-cdr! subtable (cons (cons key-2 value) (cdr subtable)))
                )
            )
            (set-cdr! (cdr table) (cons (list key-1 (cons key-2 value)) (cdr table)))
        )
    )
)



(define (make-table)

    (define (assoc key records)
        (cond ((null? records) #f)
              ((equal? (caar records) key) (car records))
              (else (assoc key (cdr records)))
        )
    )
    (let ((table (cons '*table* '())))
        (define (lookup key)
            (let ((record (assoc key table)))
                (if record
                    (cdr record)
                    #f
                )
            )
        )

        (define (insert! key value)
            (let ((record (assoc key table)))
                (if record
                    (set-cdr! record value)
                    (set-cdr! table (cons (cons (key value)) (cdr table)))
                )
            )
        )
        (define (dispatch m)
            (cond ((eq? m 'lookup) lookup)
                ((eq? m 'insert!) insert!)
                (else (error "Operation not supported" m))
            )
        )
        dispatch
    )
)









(define (make-table-2d)
    (let ((table (cons '*table* '())))
        (define (lookup key-1 key-2)
            (let ((subtable (assoc key-1 (cdr table))))
                (if subtable
                    (let ((record (assoc key-2 (cdr subtable))))
                        (if record
                            (cdr record)
                            #f
                        )
                    )
                    #f
                )
            )
        )

        (define (insert! key-1 key-2 value)
            (let ((subtable (assoc key-1 (cdr table))))
                (if subtable 
                    (let ((record (assoc key-2 (cdr subtable))))
                        (if record
                            (set-cdr! record value)
                            (set-cdr! subtable (cons (cons key-2 value) (cdr subtable)))
                        )
                    )
                    (set-cdr! table (cons (list key-1 (cons key-2 value)) (cdr table)))
                )
        
            )
        )

        (define (dispatch m)
            (cond ((eq? m 'lookup) lookup)
                  ((eq? m 'insert!) insert!)
                  (else (error "Operation not supported" m))
            )
        )
        dispatch
    )
)


(define operation-table (make-table))

(define put (operationt-table 'insert!))
(define get (operation-table 'lookup))



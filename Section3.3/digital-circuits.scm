#| 
Suppose we have the procedure make-wire already available
|#

(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))
(define e (make-wire))
(define s (make-wire))


#| 
Suppose we have the gates available (and / or / inverter)
|#
(define (half-adder a b s c)
    (let ((d (make-wire))
          (e (make-wire))
         )
         (or-gate a b d)
         (and-gate a b c)
         (inverter c e)
         (and-gate d e s)
    )
)


(define (adder a b c-in sum c-out)
    (let ((s (make-wire))
          (c1 (make-wire))
          (c2 (make-wire))
        )
        (half-adder b c-in s c1)
        (half-adder a s sum c2)
        (or-gate c1 c2 c-out )

    )
)

(define (inverter input output)
    (define (invert-input)
        (let ((new-value (logical-not (get-signal input))))
            (after-delay inverter-delay (lambda ()
                                            (set-signal! output new-value)
                                            )   
            )
        )
    )
    (set-action! input invert-input)
)

(define (logical-not input)
    (cond ((= input 1) 0)
          ((= input 0) 1)
          (else (error "Invalid signal" s))
    )
)

(define (and-gate a1 a2 output)
    (define (and-action-procedure)
        (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
            (after-delay and-gate-delay (lambda ()
                                            (set-signal! output new-value)
            ))
        )
    )
    (set-action! a1 and-action-procedure)
    (set-action! a2 and-action-procedure)
)

(define (logical-and s1 s2)
    (if (and (= 1 s1) (= 1 s2))
        1
        0
    )
)

(define (make-wire)
    (let ((signal-value 0))
         ((action-procedures '()))
         (define (set-my-signal! new-value)
            (if (not (= new-value signal-value))
                (begin (set! signal-value new-value)
                       (call-each action-procedures)
                       'done
                )
            )    
         )

         (define (accept-action-procedure! proc)
            (set! action-procedures (cons proc (action-procedures)))
            (proc)
         )

         (define (dispatch m)
            (cond ((eq? m 'get-signal) signal-value)
                  ((eq? m 'set-signal!) set-my-signal!)
                  ((eq? m 'add-action!) accept-action-procedure!)
                  (else (error "Operation not supported" m))
            )
         )
         dispatch
    )

)

(define (call-each procedures)
    (if (null? procedures)
        'done
        (begin (car procedures) (call-each (cdr procedures)))
    )
)

(define (get-signal wire)
    (wire 'get-signal)
)

(define (set-signal! wire new-value)
    ((wire 'set-signal!) new-value)
)

(define (add-action! wire action-procedure)
    ((wire 'add-action! action-procedure))
)

(define (after-delay delay action)
    (add-to-agenda! (+ delay (current-time agenda))
                    action
                    the-agend   
    )
)

(define (propagate)
    (if (empty-agenda? the-agenda) 
        'done
        (let ((first-item (first-agenda-item the-agenda)))
            (first-item)
            (remove-first-agenda-item! the-agenda)
            (propagate)
        )
    )
)

(define (probe name wire)
    (add-action! wire (lambda ()
                        (newline)
                        (display name)
                        (display " ")
                        (display (current-time the-agenda))
                        (display " New value = ")
                        (display (get-signal wire))
    ))
)


(define (make-time-segment time queue)
    (cons time queue)
)

(define (segment-time s)
    (car s)
)

(define (segment-queue s)
    (cdr s)
)

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time) (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments agenda segments) (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))
(define (empty-agenda? agenda) (null? (segments agenda)))

#| My Implementation |#


(define (add-to-agenda! time action agenda)
    (define (add-to-segment s ac)
        (insert-queue! (segment-queue s) ac)
    )

    (define (insert-segment action time before after)
        (let ((new-seg (make-time-segment time (make-queue))))
            (begin (insert-queue! (segment-queue new-seg) action)
                   (let ((new-list-obj (cons new-seg after)))
                        (set-cdr! before new-list-obj)
                    )
            )
        )
    )

    (define (add-seg curr-ptr)
        (if (null? (cdr curr-ptr))
            (insert-segment action time curr-ptr '())
            (let ((next-seg (cadr curr-ptr)))
                 (cond ((= time (segment-time next-seg)) 
                       (add-to-segment next-seg action)
                        )
                       ((> time (segment-time next-seg))
                        (add-seg (cdr curr-ptr))
                       )
                       ((< time (segment-time next-seg))
                        (insert-segment action time curr-ptr next-seg)
                       )
                 )
            )
        )
    )
)

#| Book implemnetation |#

(define (add-to-agenda! time action agenda)
    (define (belongs-before? segments)
        (or (null? segments) (< time (car segments)))
    )

    (define (make-new-time-segment time action)
        (let ((q (make-queue)))
            (insert-queue! q action)
            (make-time-segment time q)
        )
    )

    (define (add-to-segments! segments)
        (if (= segment-time (car segments) time)
            (insert-queue! (segment-queue (car segments)) action)
            (let ((rest (cdr segments)))
                (if (belongs-before? rest)
                    (set-cdr! (car segments) 
                                (cons (make-new-time-segment time action) 
                                      (cdr segments)
                                )
                    )
                    (add-to-segments rest)
                )
            )
        )
    )

    (let ((segments (segments agenda)))
        (if (belongs-before? segments)
            (set-segments! 
                agenda
                (cons (make-new-time-segment time action) segments)
            )
            (add-to-segments! segments)
        )
    )
)

(define (remove-first-agenda-item! agenda)
    (let ((q (segment-queue (first-segment agenda))))
        (delete-queue! q)
        (if (empty-queue? q)
            (set-segments! agenda (rest-segments agenda))
        )
    )
)   

(define (first-agenda-item agenda)
    (if (empty-agenda? agenda)
        (error "Agenda is empty")
        (let ((first-seg (first-segment agenda)))
            (set-current-time! agenda (segment-time first-seg))
            (front-queue (segment-queue first-seg))
        )
    )
)
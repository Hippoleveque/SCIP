#| Exercise 4.55 |#

#| a. |#

(supervisor ?x (Ben Bitdiddle))

#| b. |#
(job ?x (accounting . ?y))

#| c. |#
(address ?x (Slumerville . ?y))

#| Exercise 4.56 |#

#| a. |#

(and (supervisor ?person (Ben Bitdiddle))
     (address ?person ?where)
)

#| b. |#

(and 
    (salary ?person ?amount)
    (salary (Ben Bitdiddle) ?compared-salary)
    (lisp-value <= ?amount ?compary-salary)
)

#| c. |#

(and 
    (supervisor ?person ?supervisor)
    (not (job ?supervisor (computer . ?job-type)))
    (job ?supervisor ?supervisor-job)
)

#| Exercise 4.57 |#

(rule (can-replace ?person-1 ?person-2)
    (or (and (?job person-1)
            (?job person-2)
        )
        (and (?person-1-job person-1)
            (?job person-2)
            (?can-do ?person-1-job ?job)
        )
    )
    (not (same? ?person-1 ?person-2))
)

#| a. |#

(can-replace ?person (Cy D. Fect))

#| b. |#

(and 
    (can-replace ?person-1 ?person-2)
    (?salary-1 ?person-1)
    (?salary-2 ?person-2)
    (lisp-value
        <
        ?salary-2
        ?salary-1
    )

)

#| Exercise 4.58 |#

(rule (is-big-shot ?person)
    (and (?job ?person (?division . ?job-type))
        (not 
            (and 
                (?other-job ?supervisor (?division . ?other-job-type))
                (supervisor ?person ?supervisor)
             )
        ) 
    )
)

#| Exercise 4.59 |#

#| a. |#

(meeting ?division (Friday ?hour))

#| b. |#

(rule (meeting-time ?person ?day-and-time)
    (and 
        (?job ?person (?division . ?rest))
        (or
            (meeting ?division ?day-and-time)
            (meeting whole-company ?day-and-time)
        )
    )
)

#| c. |#

(meeting-time (Alyssa P. Hacker) (Wednesday ?time))


#| Exercise 4.60 |#

#| It happens because the relation lives-near is symetric |#

#|

Response for the second part of the question 
here : http://community.schemewiki.org/?sicp-ex-4.60
|#

#| Exercise 4.61 |#

(rule (?x next-to ?y in (?x ?y . ?u)))

(rule (?x next-to ?y in (?v . ?z))
      (?x next-to ?y in ?z)
)

#| a. |#

; Input 
(?x next-to ?y in '(1 (2 3) 4))

; Output
('1 next-to '(2 3) in '(1 (2 3) 4))
('(2 3) next-to '4 '(1 (2 3) 4))

#| b. |#

; Input
(?x next-to 1 in '(2 1 3 1))

; Ouput

('2 next-to '1 in '(2 1 3 1))
('3 next-to '1 in '(2 1 3 1))


#| Exercise 4.62 |#

(rule (last-pair '() '()))
(rule (last-pair (?x) (?x)))
(rule (last-pair '(?u . ?z) (?x))
      (last-pair ?z (?x))
)

#| Exercise 4.63 |#

(rule (grandson ?x ?y)
    (and (son ?x ?v)
         (son ?v ?y)
    )
)

(rule (son ?x ?y)
      (and 
        (wife ?w ?x)
        (son ?w ?y)
      )
)

#| Exercise 4.64 |#

#| New implementation |#

(rule (outranked-by ?staff-person ?boss)
    (or (supervisor ?staff-person ?boss)
        (and 
             (outranked-by ?middle-manager ?boss) 
             (supervisor ?staff-person ?middle-manager)          
        )
    )
)

#| Query |#

(outranked-by (Bitdiddle Ben) ?who)

#|

1. We process the input frame with ?staff-person binded to (Bitdiddle Ben)
and enter the body of the rule
2. We process separately 

    a. (supervisor (Ben Bitdiddle) ?boss) and
    b. (and 
        (outranked-by ?middle-manager ?boss) 
        (supervisor (Ben Bitdiddle) ?middle-manager)
        )
    c. In b., we first process (outranked-by ?middle-manager ?boss)
    with the input frame binding ?staff-person to (Bitdiddle Ben)
    d. It results in the processing of (outranked-by ?staff-person ?boss)
    with the input frame binding ?staff-person to ?middle-manager and so on,
    infinitely. 
It differs from the originate rule because in the original rule,
(outranked-by ?middle-manager ?boss) would take inputs frames with ?middle-manager
being binded
|#

#| Exercise 4.65 |#

#| Query |#

(wheel ?who)

#| 
    1. We enter the body of the rule wheel with the binding 
    ?person => ?who
    2. We first process the query (supervisor ?middle-manager ?person)
    3. So (supervisor ?middle-manager ?person), we get all the possible middle manager
    / person bindings as input frame for the second part od the and
    4. We have all the binding frame of middle manager / person
    5. We process (supervisor ?x ?middle-manager)
    6. For each people at the bottom, we get Warbucks Oliver

|#

#| Exercise 4.66 |#

#| 
Ben has just realized that if an instance (assertion) can be 
found through different "path" from a query, it will appear 
multiple times in the streams and be "accumulated"
just as many times. 
We need a way to either deduplicate the input streams 
of binded frames or to incorpore in the 
accumulation logic the identification of assertion 
already processed. 

|#

#| Exercise 4.67 |#

#| 
What we could do is keep in memory an object that maps 
the rule already entered with their frame. 
When entering a new loop, we check if it is already entered and 
compare the frames.
If the frames are the same, then we don't re-enter it. 
|#

#| Exercise 4.68 |#

(rule (reverse '() '()))

(rule (reverse (?x) (?x)))

(rule 
    (reverse (?u . ?v) ?z)
    (and 
        (reverse ?v ?x)
        (append-to-form ?x (?u) ?z) 
    )
)

#| 
This one will get infinite loop on (reverse (1 2 3) ?x)
|#

(rule 
    (reverse (?u . ?v) ?z)
    (and 
        (append-to-form ?x (?u) ?z) 
        (reverse ?v ?x)
    )
)
#| 
This one will get infinite loop on (reverse ?x (1 2 3))
|#

#| Exercise 4.69 |#

(rule (ends-with-grandson (grandson)))

(rule (ends-with-grandson (?u . ?v))
      (ends-with-grandson ?v)
)

(rule ((grandson) ?x ?y)
      (grandson ?x ?y)
)

(rule 
    ((great . ?rel) ?x ?y)
    (and 
        (ends-with-grandson ?rel)
        (son ?x ?z)
        (?rel ?z ?y)
    )
)

#| Exercise 4.70 |#

(define (add-assertion! assertion)
    (store-assertion-in-index assertion)
    (set! THE-ASSERTIONS 
          (cons-stream assertion THE-ASSERTIONS)
    )
    'ok
)

#| 
This implementation would create an infinite 
stream with only assertion inside because
THE-ASSERTIONS won't be evaluated. 
|#

#| Exercise 4.71 |#

(define (simple-query query-pattern frame-stream)
    (stream-flatmap
        (lambda (frame)
            (stream-append (find-assertions query-pattern frame)
                           (apply-rules query-pattern frame)
            )
        )
        frame-stream
    )
)

(define (disjoin disjunct frame-stream)
    (if (empty-disjunction? disjuncts)
        the-empty-stream    
        (interleave
            (qeval (first-disjunct disjuncts) frame-stream)
            (disjoin (rest-disjuncts disjuncts) frame-stream)
        )
    )
)

#| 
For simple-query, imagine the case where we have a very complicated
rule, instead of having to evaluate it, we delay the evaluation
of the rule to first return the assertions that
directly satisfy the query.

For disjoin
, everything would be evaluated because disjoin is calling 
itself, so if it evaluates, its going to evaluate every 
disjuncts at once
|#

#| See  http://community.schemewiki.org/?sicp-ex-4.71 for more |#

#| Exercise 4.72 |#

#| 
We use interleave to guarantee that even though
the streams could be infinite (or very long) we
will eventually reach every element.
|#

#| Exercise 4.73 |#

(define (flatten-stream stream)
    (if (stream-null? stream)
        the-empty-stream
        (interleave
            (stream-car stream)
            (flatten-stream (stream-cdr stream))
        )
    )
)

#| 
We use delay and don't implement like that
because that would mean that 
(flatten-stream (stream-cdr stream))
is recursively evaluated which would mean
evaluating the whole stream.
|#


GDI LIVE MAIL SCREENSHOT

#| Exercise 4.74 |#

#| a. |#

(define (simple-stream-flatmap proc s)
    (simple-flatten (stream-map proc s))
)

(define (simple-flatten stream)
    (stream-map
        stream-car
        (stream-filter 
            (lambda (s)
                (not (stream-null? s))
            )
        )
    )
)

#| b. |#

#| No |#

#| Exercise 4.75 |#

#| From http://community.schemewiki.org/?sicp-ex-4.75 |#

(define (uniquely-asserted pattern frame-stream)
    (stream-flatmap
        (lambda (frame)
            (let ((stream (qeval (negated-query pattern) 
                                 (singleton-stream frame))))
                (if (singleton-stream? stream)
                    stream
                    the-empty-stream
                )
            )
        )
        frame-stream
    )
)

(put 'unique 'qeval uniquely-asserted)

(define (singleton-stream? stream)
    (and (not (stream-null? stream))
         (stream-null? (stream-cdr stream))
    )
)

#| Exercise 4.76 |#

(define (conjoin conjuncts frame-stream)
    (if (empty-conjunction? conjuncts)
        the-empty-stream
        (merge (qeval (first-conjunct cunjuncts) frame-stream)
               (conjoin (rest-cunjuncts cunjuncts) frame-stream)
        )
    )
)

(define (merge s1 s2)
    (stream-flatmap
        (lambda (f1)
            (stream-filter
                (lambda (f)
                    (not (eq? f 'failed))
                )
                (stream-map
                    (lambda (f2)
                        (merge-conjoin f1 f2)
                    )
                    s2
                )
                
            )
        )
        s1
    )
)

(define (merge-conjoin f1 f2)
    (cond ((null? f1)
           f2
          )
          ((eq? f2 'failed) 'failed)
          (else
             (merge-conjoin 
                (cdr f1)
                (extend-if-possible
                    (binding-variable (car f1))
                    (binding-value (car f1))
                    f2
                )   
             )
         )
    )
)

#| Exercise 4.77 |#

#| will try later |#

#| Exercise 4.78 |#

#| Will try later |#

#| Exercise 4.79  |#

#| wont do |#
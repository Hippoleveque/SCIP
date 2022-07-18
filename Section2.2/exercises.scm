#| Exercice 2.17 |#

(define (last-pair alist)
    (if (null? (cdr alist))
        (car alist)
        (last-pair (cdr alist))
    )
)

#| Exercice 2.18 |#

(define (reverse alist)
    (if (null? alist)
        alist
        (append (reverse (cdr alist)) (list (car alist)))
    )
)

(define (reverse alist)
    (define (iter current rest)
        (if (null? rest)
            current
            (iter (cons (car rest) current) (cdr rest))
        )
    )
    (iter (list) alist)
)

#| Exercice 2.19 |#

(define (cc amount coins)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (null? coins)) 0)
          (else (+ 
                (cc amount (cdr coins)) 
                (cc (- amount (car coins)) coins)
            ))
    )
)

(define us-coins (list 50 25 10 5 1))

(cc 100 us-coins)


(cc 100 (reverse us-coins))


(define (is-same-parity a b)
     (= (remainder a 2) (remainder b 2))
)

#| Exercice 2.20 |#

(define (same-parity x . z)

    (define (iter first alist)
        (cond ((null? alist) (cons first '()))
        ((is-same-parity first (car alist)) (cons first (iter (car alist) (cdr alist))))
        (else (iter first (cdr alist)))
        )
    )
    (iter x z)
)

#| Exercice 2.21 |#

(define (square-list alist)
    (if (null? alist)
        alist
        (cons (square (car alist)) (square-list (cdr alist)))
    )
)

(define (square-list alist)
    (mapper square alist)
)


#| Exercice 2.22 |#

#| 
a. The code provided iteratively construct the list but in reverse order. 

b. The issue is that the result is not a list because the right element in the pair
is not a list itself.

|#

#| Exercice 2.23 |#

(define (for-each proc alist)
    (if (not (null? alist))
        (and (proc (car alist)) (for-each proc (cdr alist)))
    )
)

(for-each (lambda (x) (newline) (display x)) (list 1 2 3 4))

#| Exercice 2.24 |#

#| 
(list 1 (list 2 (list 3 4)))

printed by the interpreter =>  (1 ( 2 (3 4) ) )

as a box and pointer =>

| > | > null
1   | > | > null 
    2   | > | > null 
        3   4

as a tree => 

  1
<   2
  <    3            
     <   
       4
|#


#| Exercice 2.25 |#

(define x (list 1 2 (list 5 7) 9))

(= 7 (car (cdr (car (cdr (cdr x))))))

(define y (list (list 7)))

(= 7  (car (car y)))

(define z (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(= 7 (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr z)))))))))))))

#| Exercice 2.26 |#

(define x (list 1 2 3))

(define y (list 4 5 6))

#| 
(append x y) => (1 2 3 4 5 6)

(cons x y) => ((1 2 3) 4 5 6)

(list x y) => ((1 2 3) (4 5 6))
|#

#| Exercice 2.27 |#

(define (reverse alist)
    (define (iter current answer)
        (if (null? current) answer
            (iter (cdr current) (cons (car current) answer))
        )
    )
    (iter alist '())
)

(define (deep-reverse alist)
    (define (iter current answer)
        (cond ((null? current) answer)
              ((pair? (car current)) (iter (cdr current) (cons (deep-reverse (car current)) answer)))
              (else (iter (cdr current) (cons (car current) answer)))
    
        )
    )
    (iter alist '())
)

#| Exercice 2.28 |#

(define (fringe t)
    (cond ((null? t) ())
          ((pair? t) (append (fringe (car t)) (fringe (cdr t))))
          (else (list t))
    )
)


(define (fringe t)
    (define (iter current answer)
        (cond ((null? current) answer)
              ((pair? current) (iter (car current) (iter (cdr current) answer)))
              (else (cons current answer))
        )
    )
    (iter t ())
)


#| Exercice 2.29 |#

; a)
(define (make-mobile left right)
    (list left right)
)

(define (make-branch length structure)
    (list length structure)
)

(define (left-branch m)
    (car m)
)

(define (right-branch m)
    (car (cdr m))
)

(define (branch-length b)
    (car m)
)

(define (branch-structure b)
    (car (cdr b))
)

; b)

(define (total-weight m)
    (define (branch-weight b)
        (if (pair? (branch-structure b))
            (total-weight (branch-structure b))
            (branch-structure b)
        )
    )
    (+ (branch-weight (left m)) (branch-weight (right m)))
)


; c)

(define (is-balanced? m)
    (define (branch-rod b)
        (if (pair? (branch-structure b))
            (* (branch-length b) (total-weight (branch-structure b)))
            (* (branch-length b) (branch-structure b))
        )
    )
    (if (pair? m)
        (and (is-balanced? (branch-structure (left m)))
             (is-balanced? (branch-structure (right m)))
             (= (branch-rod (left m)) (branch-rod (right m)))
        )
        #t
    )
)

; d)

#| The only things that need to change are the selectors |#


#| Exercice 2.30 |#

(define (square-tree t)
    (cond ((null? t) '())
        ((pair? t) (cons (square-tree (car t)) (square-tree (cdr t))))
        (else (square t))
    )
)

(define (square-tree t)
    (define (helper x)
        (cond ((null? x) '())
            ((pair? x) (square-tree x))
            (else (square x))
        )
    )
    (mapper helper t)
)

#| Exercice 2.31 |#

(define (tree-map proc tree)
    (mapper (lambda (x) 
        (if (pair? x)
            (tree-map proc x)
            (proc x)
        )
    ) tree)
)

#| Exercice 2.32 |#

(define (subsets s)
        (if (null? s)
            (list '())
            (let ((rest (subsets (cdr s))))
                (append (mapper (lambda (x) (cons (car s) x)) rest))
            )
        )
)

#| Exercice 2.33 |#

(define (maper p seq)
    (accumulate (lambda (x y) (cons (p x) y))
                  '()
                  seq
    )
)

(define (append seq1 seq2)
    (accumulate cons
                seq2
                seq1
    )
)

(define (length seq)
    (accumulate (lambda (x y) (+ y 1))
                0
                seq
    )
)

#| Exercice 2.34 |#

(define (horner-eval x coefs)
    (accumulate (lambda (a b) (+ a (* x b)))
                0
                coefs
    )
)


#| Exercice 2.35 |#

(define (count-leaves tree)
    (accumulate (lambda (x y) (+ y 1))
                0
                (enumerate-tree tree)
    )
)

(define (count-leaves tree)
    (accumulate +
                0
                (map (lambda (node) (cond (
                    ((null? node) 0)
                    ((pair? node) (count-leaves node))
                    (else 1)
                    )))
                )
    )
)

#| Exercice 2.36 |#

(define (accumulate-n op initial seqs) 
    (if (null? (car seqs))
        '()
        (cons (accumulate op
                          initial
                          (mapper car
                                  seqs
                          )
              )
              (accumulate-n op 
                            initial 
                            (mapper cdr
                                    seqs
                            )
              )
        )
    )
)

#| Exercice 2.37 |#

(define (dot-product v w)
    (accumulate + 
                0
                (accumulate-n *
                              1
                              (list v w)
                
                )
    )
)

(define (matrix-*-vector m v)
    (mapper (lambda (x) (dot-product x v)) m)
)

(define (transpose m)
    (accumulate-n cons 
                  '()
                  m
    )
)

(define (matrix-*-matrix m1 m2)
    (mapper (lambda (x) (matrix-*-vector (transpose m2) x))
         m1
    )
)

#| Exercice 2.38 |#

; (define (fold-left op initial seq)
;     (if (null? seq)
;         initial
;         (op (fold-left op initial (cdr seq)) (car seq))
;     )
; )


; (define (fold-left op initial seq)
;     (define (iter current answer)
;         (if (null? current)
;             answer
;             (iter (cdr current) (op answer (car current)))
;         )
;     )
;     (iter seq initial)
; )

; (define (fold-right op initial seq)
;     (define (iter current answer)
;         (if (null? current)
;             answer
;             (iter (cdr current) (op (car current) answer))
;         )
;     )
;     (iter seq initial)
; )


(define (fold-right op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence) (fold-right op initial (cdr sequence)))

    )
)

(define (fold-left op initial seq)
    (define (iter result rest)
        (if (null? rest)
            result
            (iter (op result (car rest)) (cdr rest) )
        )
    )
    (iter initial seq)
)

#| 

(fold-right / 1 (list 1 2 3)) => 1 / (fold-right / 1 (list 2 3))
                              => 1 / (2 / (fold-right / 1 (list 3)))
                              => 1 / ( 2 / 3)
                              => 3 / 2

(fold-left / 1 (list 1 2 3)) => 1 / 1
                             => 1 / 2
                             => (1 / 2) / 3
                             => 1 / 6

(fold-right list '() (list 1 2 3)) => (list 1 (fold-right list '() (list 2 3)))
                                   => (list 1 (list 2 (fold-right list '() (list 3))))
                                   => (list 1 (list 2 (list 3 ())))

(fold-left list '() (list 1 2 3)) => (list (list (list '() 1) 2) 3)

|#

#| Exercice 2.39 |#

#| Base reverse |#
(define (reverse seq)
    (define (iter result rest)
        (if (null? rest)
            result
            (iter (cons (car rest) result) (cdr rest))
        )
    )
    (iter '() seq)
)


(define (reverse seq)
    (fold-right (lambda (x y) (append y (list x)))
                '()
                seq
    )
)

(define (reverse seq)
    (fold-left (lambda (x y) (cons y x)) 
                '()
                seq
    )
)

#| Exercice 2.40 |#

(define (unique-pairs n)
    (flatmap
        (lambda (i)
            (mapper (lambda (j) (list i j))
            
            (enumerate-interval 1 (- i 1)))
        )

        (enumerate-interval 2 n)
    )
)

(define (prime-sum-pairs n)
    (mapper make-pair-sum
            (filter prime-sum?
                    (unique-pairs n)
            )
    )
)

#| Exercice 2.41 |#

(define (unique-triplets n)
    (flatmap 
        (lambda (i)
            (flatmap (lambda (j)
                        (mapper (lambda (k) (list i j k))
                                (enumerate-interval 1 (- j 1))
                        ))
                    (enumerate-interval 1 (- i 1))
            )
        )
        (enumerate-interval 3 n)
    )
)

(define (triplets-sum s n)
    (filter (lambda (x)
                (= s (+ (car x) (cadr x) (caddr x)))
            )
            (unique-triplets n)
    )
)


#| Exercice 2.42 |#

(define (queens board-size)
    (define (queen-cols k)
        (if (= k 0)
            (list empty-board)
            (filter 
                (lambda (positions)
                    (safe? k positions)
                )
                (flatmap 
                    (lambda (rest-of-queens)
                        (mapper (lambda (new-row)
                                    (adjoin-position new-row k rest-of-queens)
                                )
                                (enumerate-interval 1 board-size)
                    
                        )
                    )
                    (queen-cols (- k 1))
                )
            )
        )   

    )
    (queen-cols board-size)
)

(define (adjoin-position row col rest)
    (cons (list row col) rest)
)

(define empty-board '())




((define (safe? k position)
    (define (row-safe? p)
        (not (= (car (car position)) (car p)))
    )
    (define (diag-safe? p)
        (and (not (= (- (car (car position)) (car p)) (- (cadr (car position)) (cadr p)))) (not (= (- (car (car position)) (car p)) (- (cadr p) (cadr (car position))))))
    )
    (accumulate (lambda (x y) (and x y))
                #t
                (mapper (lambda (x) (and (diag-safe? x) (row-safe? x)))
                   (cdr position)
                )
    )
))


#| Exercice 2.43 |#

#| 
 For each i we recompute queens-cols (k - 1) 

 T(n , n) = x_filter(size(T(n - 1, n))) + n * size(T(n - 1, n)) * Enum(n) + T(n - 1, n)

 =====> n^2 

 Louis'
 T(n, n) = x_filter(size(T(n - 1, n))) + n * size(T(n - 1, n)) * T(n - 1, n) + Enum(n)


 ====> n^n

|#


#| Exercice 2.44 |#

(define (split first-proc second-proc)
    (define (result-func n)
        (let ((smaller (result-func (- n 1))))
            (second-proc (first-proc smaller smaller))
        )
    )
    result-func
)

#| Exercice 2.45 |#

(define right-split (split beside below))

(define up-split (split below beside))

#| Exercice 2.46 |#

(define (make-vect x y)
    (cons x y)
)

(define (xcor-vec v)
    (car v)
)

(define (ycor-vec v)
    (cdr v)
)


(define (add-vect v1 v2)
    (make-vect (+ (xcor-vec v1) (xcor-vec v2)) (+ (ycor-vec v1) (ycor-vec v2)))
)

(define (sub-vect v1 v2)
    (make-vect (- (xcor-vec v1) (xcor-vec v2)) (- (ycor-vec v1) (ycor-vec v2)))
)

(define (scale-vect s v)
    (make-vect (* s (xcor-vec v)) (* s (ycor-vec v)))
)

#| Exercice 2.47 |#

(define (make-frame origin edge1 edge2)
    (list origin edge1 edge2)
)

(define (origin-frame frame)
    (car frame)
)

(define (edge1-frame frame)
    (cadr frame)
)

(define (edge2-frame frame)
    (caddr frame)
)

(define (make-frame origin edge1 edge2)
    (cons origin (cons edge1 edge2))
)

(define (origin-frame frame)
    (car frame)
)

(define (edge-1-frame frame)
    (cadr frame)
)

(define (edge-2-frame frame)
    (cddr frame)
)

#| Exercice 2.48 |#

(define (make-segment start end)
    (cons start end)
)

(define (start-segment s)
    (car s)
)

(define (end-segment s)
    (cdr s)
)

#| Exercice 2.49 |#

(define top-left (make-vect 0 1))
(define top-right (make-vect 1 1))
(define bottom-right (make-vect 1 0))
(define bottom-left (make-vect 0 0))

#| a. |#

(define top (make-segment top-left top-right))
(define right (make-segment top-right bottom-right))
(define bottom (make-segment bottom-right bottom-left))
(define left (make-segment bottom-left top-left))

(define outline (segment->painter (list top right bottom left)))

#| b. |#

(define positive-slope (make-segment bottom-left top-right))
(define negative-slope (make-segment top-left bottom-right))

(define cross (segment->painter (list positive-slope negative-slope)))

#| c. |#

(define middle-left (scale-vect 0.5 top-left))
(define middle-top (make-vect 0.5 1))
(define middle-right (make-vect 1 0.5))
(define middle-bottom (scale-vect 0.5 bottom-right))

(define diamond-top-left (make-segment middle-left middle-top))
(define diamond-top-right (make-segment middle-top middle-right))
(define diamond-bottom-right (make-segment middle-right middle-bottom))
(define diamond-bottom-left (make-segment middle-bottom middle-left))

(define diamond (segment->painter (list diamond-top-left diamond-top-right diamond-bottom-right diamond-bottom-left)))

#| d. |#

#| Is it that interesting ? |#

#| Exercice 2.50 |#

(define (flip-horiz painter)
    (transform-painter (make-vect 1 0)
                       (make-vect 0 0)
                       (make-vect 1 1)
    )
)

(define (rotate-180 painter)
    (transform-painter (make-vect 1 1)
                       (make-vect 0 1)
                       (make-vect 1 0)
    )
)

(define (rotate-270 painter)
    (transform-painter (make-vect 0 1)
                       (make-vect 0 0)
                       (make-vect 1 1)
    )
)

#| Exercice 2.51 |#

(define (below painter1 painter2)
    (let ((top-painter (transform-painter painter1 (make-vect 0 0.5)
                                                   (make-vect 1 0.5)
                                                   (make-vect 0 1)))
          (bottom-painter (transform-painter painter2 (make-vect 0 0)
                                                      (make-vect 1 0)
                                                      (make-vect 0 0.5)))
         )
        (lambda (frame) (top-painter frame) (bottom-painter frame))
    )
)

(define (below painter1 painter2)
    (rotate-270 (beside (rotate-90 painter1) (rotate-90 painter2)))
)

#| Exercice 2.52 |#

#| won't do |#
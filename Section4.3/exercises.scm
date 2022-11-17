#| Exercise 4.35 |#

(define (a-pythagorean-triple-between low high)
    (let ((i (an-integer-between low high)))
         (let ((j (an-integer-between i high))))
            (let ((k (an-integer between j high))))
                (require (= (+ (* i i) (* j j)) (* k k)))
                (list i j k)
    )
)

(define (an-integer-between low high)
    (require (<= low high))
    (amb low (an-integer-between (+ low 1) high))
)

#| Exercise 4.36 |#

#| 
Because it will try every possibility in the most inner loop 
which will be infinite.
|#


(define (all-pythagorean-triple-between low)
    (let ((high (an-integer-starting-from low)))
        (let ((i (an-integer-between low high)))
            (let ((j (an-integer-between i high))))
                (let ((k (an-integer between j high))))
                    (require (= (+ (* i i) (* j j)) (* k k)))
                    (list i j k)
        )
    )
)

#| Exercise 4.37 |#

(define (a-pythagorean-triple-between low high)
    (let ((i (an-integer-between low high))
          (hsq (* high high))
         )
         (let ((j (an-integer-between i high)))
            (let ((ksq (+ (* i i) (* j j))))
                (require (>= hsq ksq))
                (let ((k (sqrt ksq)))
                    (require (integer? k))
                    (list i j k)
                )
            )
        )
    )
)

#| 
In 4.35 version, we consider all ordered triplets, 
and we check that when adding the squares of the 
two lowest, we obtain the square of the highest.

In 4.37, we consider all ordered pairs,
and we check that the sum of their square
is an integer. 

I would say 4.37 explore far less possibilities.

|#


#| Exercise 4.38 |#


#| Wont do |#

#| Exercise 4.39 |#

#| 
Yes, it will be far more efficient to require first the 
more strict constraint (in the sense that it eliminates more 
solutions) than to a less strict
|#


#| Exercise 4.40 |#

#| 
Before distinct: 5 ** 5 = 3125
After disinct: 5! = 120
|#

(define (multiple-dwelling)
    (let ((baker (amb 1 2 3 4 5)))
        (require 
            (not (= baker 5))
        )
        (let ((cooper (amb 1 2 3 4 5)))
            (require 
                (not (= cooper baker))
            )
            (require 
                (not (= cooper 1))
            )
            (let ((fletcher (amb 1 2 3 4 5)))
                (require 
                    (not (= fletcher baker))
                )
                (require 
                    (not (= fletcher cooper))
                )
                (require
                    (not (= fletcher 1))
                )
                (require (not (= abs (- fletcher cooper)) 1))
                (let ((miller (amb 1 2 3 4 5)))
                    (require 
                        (not (= miller baker))
                    )
                    (require 
                        (not (= miller cooper))
                    )
                    (require 
                        (not (= miller fletcher))
                    )
                    (require
                        (> miller cooper)
                    )
                    (let ((smith (amb 1 2 3 4 5)))
                        (require 
                            (not (= smith baker))
                        )
                        (require 
                            (not (= smith cooper))
                        )
                        (require 
                            (not (= smith fletcher))
                        )
                        (require 
                            (not (= smith miller))
                        )
                        (require (not (= abs (- smith fletcher)) 1))
                        (list (list 'baker baker)
                            (list 'cooper cooper)
                            (list 'fletcher fletcher)
                            (list 'miller miller)
                            (list 'smith smith)
                        )
                    )
                )
            )
        )
    )
)

#| Exercise 4.41 |#

#| 
First, generate all permutations, then filter the ones 
that respect the constraints
|#

(define (flatmap alist)
    (if (null? alist)
        '()
        (append (car alist) (flatmap (cdr alist)))
    )
)

(define (insert el pos alist)
    (cond ((= 0 pos) (cons el alist))
          ((null? alist) (list el))
          (else (cons (car alist) (insert el (- pos 1) (cdr alist))))
    )
)

(define (range n)
    (define (iter counter)
        (if (= counter n)
            (list n)
            (cons counter (iter (+ counter 1)))
        )
    )
    (iter 0)
)

(define (list-ref alist n)
    (if (= 0 n) (car alist)
        (list-ref (cdr alist) (- n 1))
    )
)


(define (all-permutations n)
    (if (= 1 n)
        (list (list 1))
        (flatmap (map 
            (lambda (permut)
                    (map 
                        (lambda (x) (insert n x permut))
                        (range (- n 1))
                    )
                )
                (all-permutations (- n 1))
            )
        )
    )
)

(define (solve-multiple-dwellings)
    (define (is-solution possibility)
        (let ((baker-pos (list-ref possibility 0))
              (cooper-pos (list-ref possibility 1))
              (fletcher-pos (list-ref possibility 2))
              (miller-pos (list-ref possibility 3))
              (smith-pos (list-ref possibility 4))
             )
            (and 
                (not (= baker-pos 5))
                (not (= cooper-pos 1))
                (not (= fletcher-pos 1))
                (not (= fletcher-pos 5))
                (> miller-pos cooper-pos)
                (not (= (abs (- smith-pos fletcher-pos)) 1))
                (not (= (abs (- fletcher-pos cooper-pos)) 1))
            ) 
        )
    )
    (filter is-solution (all-permutations 5))
)

#| Exercise 4.42 |#

(define (solve-liars)
    (define (is-solution possibility)
        (let ((betty-pos (list-ref possibility 0))
            (ethel-pos (list-ref possibility 1))
            (joan-pos (list-ref possibility 2))
            (kitty-pos (list-ref possibility 3))
            (mary-pos (list-ref possibility 4))
            )
            (and 
                (or (and (not (= kitty-pos 2)) (= betty-pos 3))
                    (and (= kitty-pos 2) (not (= betty-pos 3)))
                )
                (or (and (not (= ethel-pos 1)) (= joan-pos 2))
                    (and (= ethel-pos 1) (not (= joan-pos 12)))
                )
                (or (and (not (= joan-pos 3)) (= ethel-pos 5))
                    (and (= joan-pos 3) (not (= ethel-pos 5)))
                )
                (or (and (not (= kitty-pos 2)) (= mary-pos 4))
                    (and (= kitty-pos 2) (not (= mary-pos 4)))
                )
                (or (and (not (= mary-pos 5)) (= betty-pos 1))
                    (and (= mary-pos 5) (not (= betty-pos 1)))
                )
            ) 
        )
    )
    (filter is-solution (all-permutations 5))
)

#| Exercise 4.43 |#



(define (solve-kansas)
    (let ((mary-ann 1)
          (melissa 2)
          (gabrielle 3)
          (lorna 4)
          (rosalind 5)
        )
        (let ((moore-daughter mary-ann)
              (barnacle-hood-daughter melissa)
              (barnacle-hood-yacht gabrielle)
              (moore-yacht lorna)
              (hall-yacht rosalind)
              (downing-yacht melissa)
            )
            (let ((hall-daughter (amb 1 2 3 4 5)))
                 (require
                    (not (= hall-daughter hall-yacht))
                 )
                (require
                    (not (= hall-daughter moore-daughter))
                )
                (require
                    (not (= hall-daughter barnacle-hood-daughter))
                )
                (let ((downing-daughter (amb 1 2 3 4 5)))
                    (require
                        (not (= downing-daughter downing-yacht))
                    )
                    (require
                        (not (= downing-daughter moore-daughter))
                    )
                    (require
                        (not (= downing-daughter barnacle-hood-daughter))
                    )
                    (require
                        (not (= downing-daughter hall-daughter))
                    )
                    (let ((parker-daughter (amb 1 2 3 4 5)))
                        (require
                            (not (= parker-daughter moore-daughter))
                        )
                        (require
                            (not (= parker-daughter barnacle-hood-daughter))
                        )
                        (require
                            (not (= parker-daughter hall-daughter))
                        )
                        (require
                            (not (= parker-daughter downing-daughter))
                        )
                        (require 
                            (or (and (= parker-daughter hall-daughter) (= hall-daughter gabrielle))
                                (and (= parker-daughter downing-daughter) (= downing-daughter gabrielle))
                            )
                        )
                        (let ((parker-yacht (amb 1 2 3 4 5)))
                            (require
                                (not (= parker-yacht parker-daughter))
                            )
                            (require
                                (not (= parker-yacht barnacle-hood-yacht))
                            )
                            (require
                                (not (= parker-yacht hall-yacht))
                            )
                            (require
                                (not (= parker-yacht downing-yacht))
                            )
                            (require
                                (not (= parker-yacht moore-yacht))
                            )
                            (list (list 'moore moore-daughter)
                                (list 'parker parker-daughter)
                                (list 'hall hall-daughter)
                                (list 'downing downing-daughter)
                                (list 'barnacle-hood barnacle-hood-daughter)
                            )
                        )
                    )
                )
            )
        )
    )
)


#| Exercise 4.44 |#

(define empty-board '())

(define (adjoin-position row col rest)
    (cons (list row col) rest)
)


(define (queens board-size)
    (define (queen-cols k)
        (if (= k 0)
            (list empty-board)
            (let ((new-row (an-integer-between 1 board-size))
                  (new-col (an-integer-between 1 board-size))
                )
                (let ((rest (queens-cols (- k 1))))
                    (let ((new-board (adjoin-position new-row new-col rest)))
                         (require (safe? k (adjoin-position new-row new-col (queen-cols (- k 1)))))
                         new-board
                    )   
                )
            )
        )
    )
    (queen-cols board-size)
)

#| Exercise 4.45 |#

#| 
The professor lectures to the student in the class with the cat.
|#

#| 

1. The professor lectures to (the student) (in the class with the cat)
The professor lectures in the (class with the cat) to the student


2. The professor lectures to (the student in the class) (with the cat)
The professor lectures with the cat to the student in the class

3. The professor lecture to (the student) (in the class) (with the cat)
The professur lectures in the class, with the cat, to the student

4. The professor elctures to ((the student) (in the class with the cat))
The professor lectures to the student that is in the class with the cat

5. The professor lectures to ((the student in the class) (with the cat))
The professor lectures to the student that is in the class and with the cat 

|#

#| Exercise 4.46 |#

#|  
As the different parsing procedures are using the *unparsed* variable,
the order of which we evaluate them will change a lot the way it 
is working.
For in instance in parse-simple-phrase, we'll have issues as the nouns won't be found
then the article will be found and then we won't parse any noun. 
|# 

#| Exercise 4.47 |#

#| 
It will call itself infinitely so it won't work. 
|#

#| Exercise 4.48 |#

#| Won't do |#


#| Exercise 4.49 |#

(define (parse-word word-list)
    (require (not (null? word-list)))
    (amb (car word-list) (parse-word (cdr word-list)))
)

#| Exercise 4.50 |#


(define (remove-from-list alist index)
    (if (= 0 index)
        (cdr alist)
        (cons (car alist) (remove-from-list alist (- alist 1)))
    )
)


(define (analyze-amb exp)
    (let ((cprocs (map analyze (amb-choices))))
        (lambda (env succeed fail)
            (define (try-next choices)
                (if (null? choices)
                    (fail)
                    (let ((random-idx (random (length choices))))
                        ((list-ref choices random-idx)
                            env
                            succeed
                            (lambda ()
                                    (try-next (remove-from-list choices random-idx))
                            )
                        )
                    )

                )
            )
            (try-next cprocs)
        )
    )
)

#| Exercise 4.51 |#

(define (permanent-assignment? exp)
    (tagged-list? 'permanent-set! exp)
)

(define (analyze-permanent-assignment exp)
    (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp)))
        )
        (lambda (env succeed fail)
            (vproc env
                   (lambda (val fail2)
                      (set-variable-value! var val)
                      (succeed 'ok
                               fail2
                      )
                   )
                   fail
            )
        )
    )
)

(define (analyze exp)
    (cond ((self-evaluating? exp) 
        (analyze-self-evaluating exp))
        ((quoted? exp)
        (analyze-quoted exp)
        )
        ((variable? exp)
        (analyze-variable exp)
        )
        ((assignment? exp)
        (analyze-assignment exp)
        )
        ((permanent-assignment? exp)
        (analyze-permanent-assignment exp)
        )
        ((definition? exp)
        (analyze-definition exp)
        )
        ((if? exp)
        (analyze-if exp)
        )
        ((lambda? exp)
        (analyze-lambda exp)
        )
        ((begin? exp)
        (analyze-sequence (begin-actions exp))
        )
        ((cond? exp)
        (analyze (cond->if exp))
        )
        ((amb? exp) (analyze-amb exp))
        ((application? exp)
        (analyze-application exp)
        )
        (else (error "Unknown expression type -- ANALYZE" exp))
    )
)

#| 
If we had used set! instead of permanent-set!, it would have displayed
1 and 1 because we backtrack direclty. 

|#


#| Exercise 4.52 |#

(define (if-fail? exp)
    (tagged-list? 'if-fail exp)
)

(define (analyze-if-fail exp)
    (let ((cproc (analyze (if-consequent exp)))
          (aproc (analyze (if-alternative exp)))
          )
          (lambda (env succeed fail) 
              (cproc env
                     succeed
                     (lambda ()
                        (aproc env
                               succeed
                               fail
                        )
                     )
              )
          )
    )
)

#| Exercise 4.53 |#

(let ((pairs '()))
    (if-fail (let ((p (prime-sum-pair '(1 2 3) '(20 35 110))))
                (permanent-set! pairs (cons p pairs))
                (amb)
            )
            pairs
    )
)

#| 
Go look at http://community.schemewiki.org/?sicp-ex-4.53, 
not clear what prime-sum-pair means. 
|#

#| Exercise 4.54 |#

(define (require? exp)
    (tagged-list? exp 'require)
)

(define (require-predicate exp)
    (cadr exp)
)

#| In analyze |#
((require? exp) (analyze-require))


(define (analyze-require exp)
    (let ((pproc (analyze (require-predicate exp))))
        (lambda (env succeed fail)
            (pproc env
                   (lambda (pred-value fail2)
                       (if (not (true? pred-value))
                           fail2
                           (succeed 'ok fail2)
                       )
                   )
                   fail
            )
        )
    )
)


#| Exercise 2.53 |#

#| 
(list 'a 'b 'c)  => (a b c)
(list (list 'george)) => ((george))
(cdr '((x1 x2) (y1 y2))) => ((y1 y2))
(cadr '((x1 x2) (y1 y2))) => (y1 y2)
(pair? (car '(a short list))) => #f
(memq 'red '((red shoes) (blue socks))) => #f
(memq 'red '(red shoes blue socks)) => '(red shoes blue socks)

|#

#| Exercise 2.54 |#

(define (equal? list1 list2)
    (cond ((and (null? list1) (null? list2)) #t)
          ((pair? (car list1)) (if (pair? (car list2))
                                   (and (equal? (car list1) (car list2)) (equal? (cdr list1) (cdr list2)))
                                   #f)
          )
          (else (and (eq? (car list1) (car list2)) (equal? (cdr list1) (cdr list2))))
    )
)

(equal? '(this is a list) '(this is a list))

(equal? '(this is a list) '(this (is a) list))

(equal? '(this (is a) list) '(this (is a) list))


#| Exercise 2.55 |#

#| 
(car ''abracadabra) is interpreted as (car (quote (quote abracadabra))) or (car (quote (quot)))

|#

#| Exercise 2.56 |#


(define (make-exponentiation base exponent)
    (cond ((and (number? base) (number? exponent)) (expt base exponent))
          ((and (number? exponent) (= 0 exponent)) 1)
          ((and (number? exponent) (= 1 exponent)) base)
          (else (list '** base exponent))
    )
)

(define (exponentiation? e)
    (and (pair? e) (eq? '** (car e)))
)

(define (base e)
    (cadr e)
)

(define (exponent e)
    (caddr e)
)

(define (deriv expr var)
    (cond ((variable? expr)
        (if (same-variable? expr var) 1 0)
        )
        ((sum? expr)
        (make-sum (deriv (addend expr) var) (deriv (augend expr) var)))
        ((product? expr)
        (make-sum (make-product (multiplier expr) (deriv (multiplicand expr) var))
                    (make-product (multiplicand expr) (deriv (multiplier expr) var)))
        )
        ((number? expr) 0)
        ((exponentiation? expr) (make-product 
                                             (make-product 
                                                           (exponent expr) 
                                                           (make-exponentiation (base expr) (- (exponent expr) 1))
                                              )
                                              (deriv (base expr) var)
                                ))
        (else (error "Unknown expression type -- DERIV" exp))
    )
)


#| Exercise 2.57 |#

(define (addend exp)
    (cadr exp)
)

(define (augend exp)
    (if (null? (cdddr exp))
        (caddr exp)
        (cons '+ (cddr exp))
    )
)

(define (multiplier exp)
    (cadr exp)
)

(define (multiplicand exp)
    (if (null? (cdddr exp))
        (caddr exp)
        (cons '* (cddr exp))
    )  
)

#| Exercise 2.58 |#

#| a. |#

(define (make-sum a1 a2)
    (cond ((and (number? a1) (number? a2)) (+ a1 a2))
          ((and (number? a1) (= 0 a1)) a2)
          ((and (number? a2) (= 0 a2)) a1)
          (else (list a1 '+ a2))
    )
)

(define (sum? exp)
    (and (pair? exp) (eq? (cadr exp) '+ ))
)

(define (addend exp)
    (car exp)
)

(define (make-product m1 m2)
    (cond ((and (number? m1) (number? m2)) (* m1 m2))
          ((and (number? m1) (= 0 m1)) 0)
          ((and (number? m1) (= 1 m1)) m2)
          ((and (number? m2) (= 0 m2)) 0)
          ((and (number? m2) (= 1 m2)) m1)
          (else (list m1 '* m2))
    )
)

(define (product? exp)
    (and (pair? exp) (eq? (cadr exp) '*))
)

(define (multiplier exp)
    (car exp)
)

#| b. |#

#| w |#


#| Exercise 2.59 |#

(define (union-set s1 s2)
    (cond ((null? s1) s2)
          ((element-of-set? (car s1) s2) (union-set (cdr s1) s2))
          (else (cons (car s1) (union-set (cdr s1) s2)))
    )
)

#| Exercise 2.60 |#

(define (adjoin-set x s)
    (cons x s)
)

(define (union-set s1 s2)
    (append s1 s2)
)

#| What it does is that it reduces the time of adjoin-set and union-set 
   but increase element-of-set? and therefore intersection-set as well

   We could use this representation in context where we do a lot of union
   and adjoin.
|#

#| Exercise 2.61 |#

(define (ajoin-set x s)
    (cond ((null? s) (list x))
          ((< x (car s)) (cons x s))
          ((> x (car s)) (cons (car s) (ajoin-set x (cdr s))))
          (else s)
    )
)

#| Exercise 2.62 |#

(define (union-set s1 s2)
    (let ((x1 (car s1))
          (x2 (car s2))
         )
        (cond ((null? s1) s2)
            ((null? s2) s1)
            ((equal? x1 x2) (cons x1 (union-set (cdr s1) (cdr s2))))
            ((> x1 x2) (cons x2 (union-set s1 (cdr s2))))
            ((< x1 x2) (cons x1 (union-set (cdr s1) s2)))
        )
    )
)

#| Exercise 2.63 |#

#| a. |#

(define (tree->list-1 tree)
    (if (null? tree) '()
                     (append (tree->list-1 (left-branch tree)) 
                             (cons (entry tree) (tree->list-1 (right-branch tree)))
                     )
    )
)

(define (tree->list-2 tree)
    (define (copy-to-list tree result-list)
        (if (null? tree)
            result-list
            (copy-to-list (left-branch tree)
                          (cons (entry tree) (copy-to-list (right-branch tree) result-list))
            )
        )
    )
    (copy-to-list tree '())
)

#| Yes, it outputs a list of the tree's element in ascending order |#

#| b. |#

#| First one has nlog(n) complexity 
    T(n) = 2 * T(n/2) + O(n)
    => T(n) = 2 * (2 * T(n/4) + O (n / 2)) + O(n)
    => T(n) = 4 * T(n/4) + 2 * O(n)
    => T(n) = 8 * T(n/4) + 3 * O(n)
    => T(n) = O(nlog(n))

    Second one has n complexity
    T(n) = 2 * T(n/2) + O(1)
    T(n) = O(n)
|# 

#| Exercise 2.64 |#

(define (list->tree elements)
    (car (partial-tree elements (length elements)))
)

(define (partial-tree elts n)
    (if (= n 0)
        (cons '() elts)
        (let ((left-size (quotient (- n 1) 2)))
            (let ((left-result (partial-tree elts left-size)))
                (let ((left-tree (car left-result))
                      (non-left-elts (cdr left-result))
                      (right-size (- n (+ left-size 1)))
                      )
                     (let ((this-entry (car (non-left-elts)))
                           (right-result (partial-tree (cdr non-left-elts) right-size))
                          )
                          (let ((right-tree (car right-result))
                                (remaining-elts (cdr right-result))
                               )
                               (cons (make-tree this-entry left-tree right-tree) remaining-elts)
                          )
                    )
                )
            )
        )
    )
)

#| a. |#

#| 
We construct the left-tree from the left-half of the list, 
we get a pair (left-tree rest), then we get the first element of the rest
to be the root of the tree and construct the right tree from (cdr rest).
As we always what remains of the list, we need to get the sizes and store
the remaining els at each step
|#


#| b. |#

#|  
    We have clearly 
    T(n) = 2 * T(n / 2) + O(1)
    ===>  The complexity is O(n)
|#


#| Exercise 2.65 |#

(define (union-ordered-list list1 list2)
        (cond ((null? list1) list2)
            ((null? list2) list1)
            ((> (car list1) (car list2)) (cons (car list2) (union-ordered-list list1 (cdr list2))))
            ((< (car list1) (car list2)) (cons (car list1) (union-ordered-list (cdr list1) list2)))
            ((= (car list1) (car list2)) (cons (car list1) (union-ordered-list (cdr list1) (cdr list2))))
        )
)


(define (intersection-ordered-list list1 list2)
        (cond ((or (null? list1) (null? list2)) '())
            ((> (car list1) (car list2)) (intersection-ordered-list list1 (cdr list2)))
            ((< (car list1) (car list2)) (intersection-ordered-list (cdr list1) list2))
            ((= (car list1) (car list2)) (cons (car list1) (intersection-ordered-list (cdr list1) (cdr list2))))
        )
)



(define (union-set set1 set2)
    (list->tree (union-ordered-list (tree->list-2 set1) (tree->list-2 set2)))
)

(define (intersection-set set1 set2)
    (list->tree (intersection-ordered-list (tree->list-2 set1) (tree->list-2 set2)))
)

#| Exercise 2.66 |#

(define (lookup-binary given-key records)
    (cond ((null? records) #f)
          ((> given-key (key (car records))) (lookup-binary given-key (right-branch records)))
          ((< given-key (key (car records))) (lookup-binary given-key (left-branch records)))
          ((= given-key (key (car records))) (car records ))
    )
)

#| Exercise 2.67 |#

(define sample-tree 
    (make-code-tree (make-leaf 'A 4)
                    (make-code-tree  (make-leaf 'B 2)
                                     (make-code-tree (make-leaf 'D 1)
                                                     (make-leaf 'C 1)
                                     )
                    )
    )
)

#| Exercise 2.68 |#

(define (encode message tree)
    (if (null? message)
        '()
        (append (encode-symbol (car message) tree)
                (encode (cdr message) tree)
        )
    )
)

(define (element-of-list? symbol list)
    (cond ((null? list) #f)
          ((eq? (car list) symbol) #t)
          (else (element-of-list? symbol (cdr list)))
    )
)

(define (encode-symbol symbol tree)
    (cond ((leaf? tree) '())
          ((element-of-list? symbol (symbols (left-branch tree))) (cons 0 (encode-symbol symbol (left-branch tree))))
          ((element-of-list? symbol (symbols (right-branch tree))) (cons 1 (encode-symbol symbol (right-branch tree))))
          (else (error "symbol is not in the tree"))
    )
)

#| Exercise 2.69 |#

(define (generate-huffman-tree pairs)
    (successive-merge (make-leaf-set pairs))
)

(define sample-pairs (list (list 'A 4) (list 'B 2) (list 'C 1) (list 'D 1)))


(define (successive-merge leaves)
    (if (null? (cdr leaves))
        (car leaves)
        (successive-merge (adjoin-set (make-code-tree (cadr leaves)
                                                      (car leaves)
                                      )
                                      (cddr leaves)
                          )
        )
    )
)

#| Exercise 2.70 |#

(define rock-pairs (list (list 'A 2) (list 'BOOM 1) (list 'GET 2)
                         (list 'JOB 2) (list 'NA 16) (list 'SHA 9)
                         (list 'YIP 9) (list 'WAH 1)
                   )
                    
)

(define message '(Get a job Sha na na na na na na na na Get a job Sha na na na na na na na na Wah yip yip yip yip yip yip yip yip yip Sha boom)
)

#| 
Length of the encoded message : 90
Length of the fixed-length code enconded message: 36 * 3 = 108
|#

#| Exercise 2.71 |#

#| 

We will end up with a tree where all left branches are leaves
1 bit will be necessary for encoding the most frequent, then 2 bits
for the second most frequent and finally n-1 for the least-frequent

|#


#| Exercise 2.72 |#

#| 
For enconding a symbol:
1. Position at the root of the tree
2. Parse the symbols on the left and right branches O(num-leaves-of-the-tree)
3. Go to the procedure again until we reach the right leaf

Special case of ex 2.71,
It would require O(1) to encode the most frequent element 
(Only one symbol on the left-branch to parse)
It would require O(n-1 + n-2 + n-3 + ... + 1) = O(n**2)
To encode the least frequent element

|#
(define (make-leaf symbol weight)
    (list 'leaf symbol weight)
)

(define (leaf? object)
    (eq? (car object) 'leaf)
)

(define (symbol-leaf object)
    (cadr object)
)

(define (weight-leaf object)
    (caddr object)
)

(define (make-code-tree left right)
    (list left 
          right 
          (append (symbols left) (symbols right))
          (+ (weight left) (weight right))
    )
)

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
    (if (leaf? tree)
        (list (symbol-leaf tree))
        (caddr tree)
    )
)

(define (weight tree)
    (if (leaf? tree)
        (weight-leaf tree)
        (cadddr tree)
    )
)

(define (decode bits tree)
    (define (decode-1 bits current-branch)
        (if (null? bits)
            '()
            (let ((next-branch (chose-branch (car bits) current-branch)))
                (if (leaf? next-branch) 
                    (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
                    (decode-1 (cdr bits) next-branch)
                )
            )   
        )
    )
    (decode-1 bits tree)
)

(define (chose-branch bit tree)
    (cond ((= bit 0) (left-branch tree))
          ((= bit 1) (right-branch tree))
          (else (error "bad bit" bit))
    )
)


(define (adjoin-set x set)
    (cond ((null? set) (list x))
          ((> (weight x) (weight (car set))) (cons (car set) (adjoin-set x (cdr set))))
          (else (cons x set))      
    )
)

(define (make-leaf-set pairs)
    (if (null? pairs)
        '() 
        (let ((pair (car pairs)))
            (adjoin-set (make-leaf (car pair) (cadr pair))
                        (make-leaf-set (cdr pairs))
            )
        )
    )
)
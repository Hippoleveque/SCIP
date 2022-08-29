#| Exercice 3.38 |#

#| 

a. There is six possible ordering 
of these operations

1. 1 2 3 =>  110 -> 90 -> 45

2. 1 3 2 => 110 -> 55 -> 35

3. 2 1 3 => 80 -> 90 -> 45

4. 2 3 1 => 80 -> 40 -> 20

5. 3 1 2 => 50 -> 60 -> 40

6. 3 2 1 => 50 -> 30 -> 40


b. Won't do the diagram, 
this is completely redundant from the course.

As an example, we could have 
Mary and Paul's operations interleaving

=> Mary Withdraw 50$ and Paul 10$
at the end, the account has 90 so that 
the total amount of money in circulation
is 150$



|#



#| Exercice 3.39 |#

#| 

Situation 1. and 2. can happen
Situation 3. cannot happen cannot happen
because the two evaluation are guarateed to be the same
Situation 4. Cannot happen because P1 needs to finish
before P2 can proceed
Situation 5. Can happen

|#

#| Exercice 3.40 |#

#| 

1 -> 1 000 000 : P1 -> 100 then P2 -> 1 000 000

2 -> 1 000 000 : P2 -> 1000 then P1 -> 1 000 000

3 -> 100 : P1 accesses (2) x, then P2 accesses (3) x,
sets it to 1000, then P1 sets x to 100

4 -> 1 000 : P2 accesses (3) x, then P1 accesses (2) x
and sets it to 100, then P2 sets x to 1 000

5 -> 10 000 : P1 accesses (1) x, then P2 accesses (3) x and
sets it to 1 000, then P1 accesses (1) x and sets 
it to 10 * 1 000 = 10 000

6 -> 100 000 : P2 accesses (1) x, then P1 accesses (2) x and sets
it to 100, then P2 accesses (2) x, then sets it to 
10 * 100 * 100 = 100 000

7 -> 10 000, P2 accesses (2) x, then P1 accesses (2) x and sets
it to 100, then P2 accesses (1) x, then sets it to 
10 * 10 * 100 = 10 000

|#

#| Exercice 3.41 |#

#| 
As there is only this system that can access the variable
balance, there won't be any scenario where this can 
cause an anomalous behavior.
What can happen though is that you the system 
that access the balance can end up with a value that 
will immediately be out of date, but the balance
variable will keep its consistency 
|#

#| Exercice 3.42 |#

#| 
It will prevent interleaving between a deposit and 
a withdraw but it is not clear wether it will 
prevent interleaving between two deposits or two withdraws
serializer will consider it as the same procedure and 
we do not know if a procedure can interleave with itself
|#

#| Exercice 3.43 |#

#| 
By reccurence 
At step 0 the accounts amount 
are indeed 10 20 30 

If at step k the accounts amounts
are 10 20 30 in some order
Then let's say that 

account a : 10
account b : 20
account c : 30

The possible exchange are :
account a <> b, result
account a: 20
account b : 10
account c : 30

account a <> c, result
account a : 30
account b: 20
account c : 10

account b <> c, result
account a : 10
account b : 30
account c : 20

So in any case the accounts amount
are still 10, 20 30 in some
order.

Without drawing a diagram, 

With the first implementation, 
if we exchange a <> b concurrently with b <> c

Then what might happen : 

E1 accesses a = a0 
E1 accesses b = b0 
E2 accesses b = b0  
E2 accesses c = c0
E2 swaps c & b => b = c0 c = b0
E1 sets a = b0 
E1 sets b = c0 - b0 + a0  

at the end c = b0
           a = b0
           b = c0 - b0 + a0 

The condition described above are violated

a + b + c = a0 + b0 + c0 


If we do not serialize individual account

E1 accesses a = a0
E1 accesses b = b0
E2 accesses b = b0
E2 accesses c = c0
E2 sets c to b0
E1 sets a to b0 
E2 sets b to c0
E1 sets b to a0

at the end c = b0
           a = b0
           b = a0
|#

#| Exercice 3.44 |#

#| 

There is no problem 
because there is no dependency 
on what the initial accounts amounts 
where exactly for doing this transfer 
(their states depend on one another)
(except that we first account might be lacking
funds but that is not taken into account
by hypothesis)

|#

#| Exercice 3.45 |#

#| 

What would happen in serialized-exchange is that: 

- both exchange and the withdraw  for account 1 are serialized by account1's serializer
- both exhange and the deposit for account 2 are serialized by account2's serializer

=> Exchange starts, it accesses account1 and account2 balance
=> It wants to starts account1's withdraw but need to wait until 
the exchange procedures finishes because they are part of the same serializer
=> Exchange can't finish 

|#


#| Exercice 3.46 |#

#| 

Won't draw the diagram because an explanation is enough

If we implement it just like this, we could fall into the following scenario:

> Serialized Procedure 1 (S1) tries to acquire the mutex: 
> The mutex acquire procedure is launched, test-and-set! is launched
> The cell is false
> S2 tries to acquire the mutex, test-and-set! is launched
> The cell is false
> S1 (or S2) sets the cell to true
> S2 (or S1) sets the cell to true
> They both 'think' that they have acquired the mutex and we fall into the
same issue as described previously

|#


#| Exercice 3.47 (Bad implementation)|#

#| a. |#

(define (make-semaphore n)
    (let ((mutexes (create-mutexes n)))
        (let ((current-mutex mutexes))
            (define (the-semaphore m)
                (cond ((eq? m 'acquire)
                       (let ((mut (car current-mutex)))
                            (if (null? (cdr current-mutex))
                                (begin (mut 'acquire) (set! current-mutex mutexes) mut)
                                (begin (mut 'acquire) (set! current-mutex (cdr current-mutex)) mut)
                            )
                       )
                    )
                    ((eq? m 'release) (lambda (mut) (mut 'release)))
                )
            )
        )
    )
)

#| 

To use it, we need to store somewhere the mutex that has been acquired
 
|#


#| b. |#

(define (make-semaphore n)
    (let ((cells (make-false-list n)))

        (define (release x))
        (define (the-semaphore m)
            (cond ((eq? m 'acquire)
                   (if (test-and-set-semaphore! cells)
                       (the-semaphore 'acquire)
                    )
                  )
                  ((eq? m 'release) (lambda (cls) (clear! cls)))
        
            )
        )
    )
)

(define (make-false-list n)
    (if (= 0 n)
        '()
        (cons false (make-false-list (- n 1)))
    )
)

(define (test-and-set-semaphore! cells)
    (define (iter rest)
        (cond ((null? rest)
               (iter cells)
              )
              ((test-and-set! rest) (iter (cdr rest)))
              (else rest)
    
        )
    )
    (iter cells)
)



#| Exercice 3.47 (Good implementation)|#

#| a. |#

(define (make-semaphore n)
    (let ((mut (mutex))
         ((numClients 0)))
         (define (the-semaphore m)
            (cond ((eq? m 'acquire)
                   (mut 'acquire)
                    (if (> n numClients)
                        (set! numClients (+ numClients 1))
                        ((mut 'release) (the-semaphore 'acquire))
                    )
                    (mut 'release)

                  )
                  ((eq? m 'release)
                    (mut 'acquire)
                    (set! numClients (- numClients 1))
                    (mut 'release)
                  )
            )
         )
    )
)

#| b. |#

(define (make-semaphore n)
    (let ((mut (list false))
          ((numClients 0))
         )
         (define (the-semaphore m)
            (cond ((eq? m 'acquire)
                    (if (test-and-set! mut)
                        (the-semaphore 'acquire)
                        (set! numClients (+ numClients 1))
                        (clear! mut)
                    )
                  )
                  ((eq? m 'release)
                    (if (test-and-set! mut)
                        (the-semaphore 'release)
                        (set! numClients (- numClients 1))
                        (clear! mut)
                    )
                  )
            )
         )
    )
)


#| Exercies 3.48 |#

#| 
In the situation described above, if Paul and Peter attempt
to concurrently exchange a1 with a2 and a2 with a1, they will
both first try to acquire the mutex for a1 so that they won't
stuck because one of them will proceed first, then proceed
to a2 and leave the other one finish as well. 
|#

(define (make-account balance id)

    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                   balance)
            "Insufficient funds"
        )
    )
    (define (deposit amount)
        (begin (set! balance (+ balance amount)) balance)
    )
    (let ((balance-serializer (make-serializer)))
        (define (dispatch m)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  ((eq? m 'serializer) balance-serializer)
                  ((eq? m 'id) id)  
                  (else (error "Unsupported request" m))
            )
        )
        dispatch
    )
)

(define (serialized-exchange account1 account2)
    (let ((serializer1 (account1 'serializer))
          (serializer2 (account2 'serializer))
          (id1 (account1 'id))
          (id2 (account2 'id))
         )
        (if (< id1 id2)
            ((serializer1 (serializer2 exchange)) account1 account2)
            ((serializer2 (serializer1 exchange)) account1 account2)
        )
    )
)

#| Exercies 3.49 |#

#| 
Say that an account contains both a balance
and the id of another account

We design a procedure exchange with destination 
that exchange the balance of the account with 
the other account that it points to. 

Suppose a1 points to a2 and a2 points to a1. 

We concurrently run the process for both. 

S1 has locked a1 and wants to access a2 to perform the exchange.
S2 has locked a2 and wants to access a1 to perform the exchange.

|#
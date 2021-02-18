; Define the first procedure and name it smallest-divisor
; The formal parameters of this procedure are n
; The body of this procedure call another procedure called find-divisor

; Define the second procedure and name it find-divisor
; the formal parameters of this procedure are n and test-divisor
; the body of this procedure is made up of a condition that has 3 clauses
; 
; The first of these clauses has a predicate that tests to see if the square
; of the test-divisor is greater than the value of the smallest divisor we are 
; testing. If this predicate is true all possible values for the smallest-divisor
; will have been exhausted so the consequent is returned which in this case is
; the smallest divisor (which in turn through abstraction is the prime-number).
;
; The second of these clauses has the predicate that tests to see if the 
; test-divisor is divisible by the smallest-divisor we are test. 
; if the predicate is true the consequent returns the test-divisor which means
; we have found a divisor (which in turn means the number we are testing is not)
; 
; The third of these claues has the predicate 'else' which always evaluates to
; true if it is reached. This means the condition evaluates the final expression
; rather than being undefined. The final expression which recursively calls the 
; find-divisor definition however the the test divisor formal parameter has 
; been incremented by 1.
;
; This process is iterative be cause we know the value of all the state variables
; throughout the process. Nothing is deferred for later evaluation.

(define (square a) 
  (* a a))

(define (smallest-divisor n) 
  (find-divisor n 2))

(define (divides? a b) 
  (= (remainder b a) 0))

(define (find-divisor n test-divisor) 
  (cond ((> (square test-divisor) n) n) 
        ((divides? test-divisor n) test-divisor) 
        (else (find-divisor n (+ test-divisor 1)))))

(define (prime? n) 
  (= n (smallest-divisor n)))


(prime? 1)
(prime? 2)
(prime? 3)
(prime? 4)
(prime? 5)
(prime? 6)
(prime? 7)
(prime? 8)
(prime? 9)
(prime? 10)
(prime? 11)
(prime? 12)
(prime? 13)
(prime? 14)
(prime? 15)
(prime? 16)
(prime? 17)
(prime? 18)
(prime? 19)
(prime? 20)
(prime? 21)
(prime? 22)
(prime? 23)
(prime? 24)
(prime? 25)

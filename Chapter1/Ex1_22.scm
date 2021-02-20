; Delete the below section and see if you can use logic and memory to reformulate it

(define (prime? n) 
  (= (smallest-divisor 2 n) n))

(define (smallest-divisor test-divisor n) 
  (cond ((> (square test-divisor) n) n) 
        ((divides? test-divisor n) test-divisor) 
        (else (smallest-divisor (+ test-divisor 1) n))))

(define (divides? a b) 
  (= (remainder b a) 0))

(define (square x) 
  ( * x x))

(prime? 10)
(prime? 11)

; Ok now we have the definitions let's now lexically scope prime?

(define (prime? n) 
  (define (square x) 
    ( * x x))
  (define (divides? a b) 
    (= (remainder b a) 0))
  (define (smallest-divisor test-divisor) 
    (cond ((> (square test-divisor) n) n) 
          ((divides? test-divisor n) test-divisor) 
          (else (smallest-divisor (+ test-divisor 1)))))
  (= (smallest-divisor 2) n))


(prime? 10)
(prime? 11)

; Now we have a definition for prime we can run the tests
; First we must get the functions below working 

(define (timed-prime-test n) 
  (newline) 
  (display n) 
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time) 
  (if (prime? n) 
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time) 
  (display " *** ") 
  (display elapsed-time))

(timed-prime-test 2383)


; So no we have the code working it is now time to get the program to run

(define (odd? low) 
  (not (= (remainder low 2) 0)))


(odd? 3)
(odd? 4)


(define (search-for-primes low high) 
  (cond ((and (< low high) (odd? low)) 
              (timed-prime-test low)
              (search-for-primes (+ low 2) 
                             high)) 
        ((< low high) 
         (timed-prime-test low)
          (search-for-primes (+ low 1) 
                             high)) 
        (else (newline) (display "Done!"))))
        
(search-for-primes 1000 2000) ; Test Range A

(search-for-primes 10000 20000) ; Test Range B

(search-for-primes 100000 200000) ; Test Range C

;(search-for-primes 1000000 2000000) ; Test Range D

;(search-for-primes 100000000 200000000) ; Test Range E

; Test Range C is 10 times the size of Test Range A
; It takes roughly 10 times longer to check for primes in Range C compared to
; Range A

; 10 root 10 is about 32 which is the time it takes to test primes in Range D
; This is about right when compared to a. 

; 1000 root 10 is 3162. Test Range E



; Now we have a working function that iterates through what we want we need
; to find the first 3 primes for a given range

; We could create a function for finding the 3 first primes after 1000, 10000
; ... and so on. I'm not going to do that because cba. 
; Actually changed my mind. the number 50 is arbitrarily chosen

(+ 3 4)

(define (search-for-first3-primes low) 
  (search-for-primes low (+ low 100)))

(search-for-first3-primes 1000)

(search-for-first3-primes 10000)

(search-for-first3-primes 100000)

(search-for-first3-primes 1000000)

(search-for-first3-primes 10000000)

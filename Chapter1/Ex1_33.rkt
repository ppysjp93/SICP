(define (accumulate combiner null-value term a next b) 
  (if (> a b) 
      null-value
      (combiner (term a) 
                (accumulate combiner null-value term (next a) next b))))

; let's adapt accumulate so that we have filter accumulate

(define (filtered-accumulate filter combiner null-value term a next b) 
  (cond ((> a b) null-value)  
        ((filter a) (combiner (term a) 
                              (filtered-accumulate filter combiner null-value 
                                                   term (next a) next b)))
        (else (filtered-accumulate filter combiner null-value 
                                   term (next a) next b))))

; Let's test the 2nd higher order function on something trivial

(define (sum-odd-ints a b) 
  (define (filter x) 
    (not (= (remainder x 2) 0)))
  (define (combiner current previous) 
    (+ current previous))
  (define (term x) 
    x)
  (define (next x) 
    (+ x 1))
  (filtered-accumulate filter combiner 0 term a next b))

(sum-odd-ints 1 100)

; We can find the prime number by finding the smallest divisor
; if the smallest divisor is the prime number then we have a prime
; otherwise we don't have a prime

(define (prime? n) 
  (= (smallestdivisor n) n))

; We now need to define how smallest divisor will work
; We know we want it to iterate up to the square root of n,
; if smallestdivisor squared is greater than n we want it to return n
; other wise we want to test the value we are using to test for smallestdivisor
; when we take the remainder n value, it returns a value other than 0 we have 
; a divisor d. Other wise we want to keep testing.

(define (smallestdivisor n) 
  (define (square x) 
    (* x x))
  (define (next x) 
    (+ x 1))
  (define (smallestdivisor-iter val n)
      (cond ((> (square val) n) n)
            ((= (remainder n val) 0) val)
            (else (smallestdivisor-iter (next val) n))))
  (smallestdivisor-iter 2 n))

(smallestdivisor 9)

; In principle we now have a way of finding primes. There a are better ways
; but you did do this from memory/logic. Pretty good.



; Ok so now let's use our prime filter to sum the squares of the primes between
; a and b

(define (sum-cube-primes a b) 
  (define (prime? n) 
    (define (smallestdivisor n) 
      (define (square x) 
        (* x x))
      (define (next x) 
        (+ x 1))
      (define (smallestdivisor-iter val n)
          (cond ((> (square val) n) n)
                ((= (remainder n val) 0) val)
                (else (smallestdivisor-iter (next val) n))))
    (smallestdivisor-iter 2 n))
  (= (smallestdivisor n) n))
  (define (combiner current previous) 
    (+ current previous))
  (define (term x) 
    (* x x x))
  (define (next x) 
    (+ x 1))
  (filtered-accumulate prime? combiner 0 term a next b))

(sum-cube-primes 1 10)


; So the GCD can be found with a recursive formula. 
(define (gcd x y) 
  (if (= y 0) 
      x
      (gcd y (remainder x y))))

; ok so the filter we are going to apply is called relative-prime?

(define (relative-prime? a n) 
  (= (gcd a n) 1))

; now we are ready to put our functions into our filtered-accumulate

(define (product-relative-primes a b) 
 (define (gcd x y) 
  (if (= y 0) 
      x
      (gcd y (remainder x y))))  
  (define (relative-prime? x y) 
    (= (gcd x y) 1))
  (define (combiner current previous) 
    (* current previous))
  (define (term x) 
    x)
  (define (next x) 
    (+ x 1))
  (filtered-accumulate relative-prime? combiner 1 term a next b))

; You may be lulled into thinking that that the accumulator doesn't need 
; changing but it does. The filter needs to take to parameters now so we need
; to update our second higher order function 

(define (filtered-accumulate filter combiner null-value term a next b) 
  (cond ((> a b) null-value)  
        ((filter a b) (combiner (term a) 
                              (filtered-accumulate filter combiner null-value 
                                                   term (next a) next b)))
        (else (filtered-accumulate filter combiner null-value 
                                   term (next a) next b))))

(product-relative-primes 1 10)

; Let's double check the result

(gcd 1 10)
(gcd 2 10)
(gcd 3 10)
(gcd 4 10)
(gcd 5 10)
(gcd 6 10)
(gcd 7 10)
(gcd 8 10)
(gcd 9 10)
(gcd 10 10)

; We find that 3 7 and 9 are relatively prime to 10
; If we multiply these values together then we get 189 too.




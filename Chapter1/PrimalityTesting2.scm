(define (square x) 
  (* x x))

(define (even? x) 
  (= (remainder x 2) 0))

(define (expmod base exp m) 
  (cond ((= exp 0) 1) 
        ((even? exp) 
         (remainder (square (expmod base (/ exp 2) m)) 
                    m)) 
        (else 
          (remainder (* base (expmod base (- exp 1) m)) 
                     m))))
(square 3)

(even? 4)

(expmod 4 23 23)

; since 23 is greater than 4, we know that 4 mod 23 is just 4
; it does make you wonder why they say it is co

(= (expmod 4 23 23) 4)

(define (try-it a n) 
  (= expmod a n n) a)


(define (fermat-test n)
  (define (try-it a) 
    (= (expmod a n n) a)) 
  (try-it (+ 1 (random (- n 1)))))

; the above has been lexically scoped. 

(fermat-test 9)
(fermat-test 11)

; fermat-test only tests once. If we test a number of times, the probability that
; we test a prime correctly improves because there are more chances for us 
; to find a incongruent modulo n.

(define (fast-prime? n times) 
  (cond ((= times 0) true) 
        ((fermat-test n) (fast-prime? n (- times 1))) 
        (else false)))


(fast-prime? 1000 100)


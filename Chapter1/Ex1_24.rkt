; Original Definition for prime that has root n growth

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

; Definition of prime that has logarithmic growth

(define (expmod base exp m) 
  (define (square x) 
    (* x x))
  (define (even?) 
    (= (remainder exp 2) 0))
  (cond ((= exp 0) 1) 
        ((even?) 
         (remainder (square (expmod base (/ exp 2) m)) 
                    m)) 
        (else 
          (remainder (* base (expmod base (- exp 1) m)) 
                     m))))

(define (try-it a n) 
  (= expmod a n n) a)

(define (fermat-test n)
  (define (try-it a) 
    (= (expmod a n n) a)) 
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times) 
  (cond ((= times 0) true) 
        ((fermat-test n) (fast-prime? n (- times 1))) 
        (else false)))

; Code used to measure the time taken for each prime

(define (timed-prime-test n) 
  (newline) 
  (display n) 
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time) 
  (if (fast-prime? n 1) 
      (report-prime (- (runtime) start-time))))

(define (start-prime-test n start-time) 
  (if (prime? n) 
      (report-prime (- (runtime) start-time))))


(define (report-prime elapsed-time) 
  (display " *** ") 
  (display elapsed-time))

(timed-prime-test 1009)
(timed-prime-test 1013)
(timed-prime-test 1019)
(timed-prime-test 10007)
(timed-prime-test 10009)
(timed-prime-test 10037)
(timed-prime-test 100003)
(timed-prime-test 100019)
(timed-prime-test 100043)
(timed-prime-test 1000003)
(timed-prime-test 1000033)
(timed-prime-test 1000033)

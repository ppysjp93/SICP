(define (square x) 
  (* x x))



(define (find-divisor n test-divisor) 
  (cond ((> (square test-divisor) n) n) 
        ((divides? test-divisor n) test-divisor) 
        (else (find-divisor n (+ test-divisor 1)))))


(define (divides? a b) 
  (= (remainder b a) 0))

(divides? 3 2)
(divides? 4 8)


(define (prime? n) 
  (= n (smallest-divisor n)))

(prime? 11)

(define (smallest-divisor n) 
  (find-divisor n 2))


(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

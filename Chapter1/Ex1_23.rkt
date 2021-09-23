; This is the Lexicaly scoped prime procedure from Ex1_22.scm
(define (prime? n) 
  (define (smallest-divisor test-divisor) 
    (define (divides?) 
        (= (remainder n test-divisor) 0))
    (define (square) 
        ( * test-divisor test-divisor))
    (cond ((> (square) n) n) 
          ((divides?) test-divisor) 
          (else (smallest-divisor (+ test-divisor 1)))))
  (= (smallest-divisor 2) n))

; Below is the definiton of next asked for in the question
; This definition of next works so thats great. We can shove the definition
; Of next into smallest-divisor and lexically scope it.

(define (next test-divisor) 
  (if (= test-divisor 2) 
      3
      (+ test-divisor 2)))

; upgraded prime because of next but still not logarithmic

(define (prime? n) 
  (define (smallest-divisor test-divisor) 
    (define (divides?) 
        (= (remainder n test-divisor) 0))
    (define (square) 
        ( * test-divisor test-divisor))
    (define (next) 
        (if (= test-divisor 2) 
            3
            (+ test-divisor 2)))
    (cond ((> (square) n) n) 
          ((divides?) test-divisor) 
          (else (smallest-divisor (next)))))
  (= (smallest-divisor 2) n))

(define (search-for-primes low high) 
  (define (odd?) 
    (not (= (remainder low 2) 0)))
  (define (timed-prime-test) 
    (define (start-prime-test start-time) 
      (define (report-prime elapsed-time) 
        (display " *** ") 
        (display elapsed-time))
      (if (prime? low) 
          (report-prime (- (runtime) start-time))))
    (newline) 
    (display low) 
    (start-prime-test (runtime)))
  (cond ((and (< low high) (odd?)) 
              (timed-prime-test)
              (search-for-primes (+ low 2) 
                             high)) 
        ((< low high) 
         (timed-prime-test)
          (search-for-primes (+ low 1) 
                             high)) 
        (else (newline) (display "Done!"))))
        
; So now we can do the testing.
; We test by loading up our first defition of prime? and compare it to our 
; second definiton of prime. 

(define (search-for-first3-primes low) 
  (search-for-primes low (+ low 100)))

(search-for-first3-primes 525)

(define (even? n) 
  (if (= (remainder n 2) 0) true false))

(define (square x) 
  (* x x))

(define (expmod base exp m) 
  (cond ((= exp 0) 1) 
        ((even? exp) 
         (remainder (square (expmod base (/ exp 2) m)) 
                    m)) 
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test2 n) 
  (define (try-it a n) 
   (cond ((= a (- n 1)) true) 
         ((= (expmod a n n) a) 
          (try-it (+ a 1) n)) 
         (else false)))  
  (try-it 2 n))

(fermat-test2 561)
(fermat-test2 562)
(fermat-test2 563)

; 561 isn't a prime and yet it passes the fermat-test, hence it is a Carmicheal 


(fermat-test2 1105)
(fermat-test2 1729)
(fermat-test2 2465)
(fermat-test2 2821)
(fermat-test2 6601)


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



(define (carmicheal-test n) 
  (define (try-it a) 
   (cond ((> a n) true) 
         ((not (= (expmod a n n)) a) false) 
         (else (try-it (+ a 1))))) 
  (try-it 2))

(carmicheal-test 561)


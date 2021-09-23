(define (even? n) 
  (if (= (remainder n 2) 0) true false))

(define (square x) 
  (* x x))


; So these are the two procedures separated out
; I think these need combining in the way that expmod was 
; a combination of two procedures

(define (square_equal_1_modulo_n? n) 
  (if (and (not (= n 1))
           (not (= n (- n 1))) 
           (= (remainder (square n) 1) 1)) 0))

(define (expmod base exp m) 
  (cond ((= exp 0) 1) 
        ((even? exp) 
         (remainder (square (expmod base (/ exp 2) m)) 
                    m)) 
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

; Square check is a combining of the remainder and square functions.
; Square check tells us if the event that we have found a non-trivial square
; whose modulo n is 1


(define (square-check n m) 
  (if (and (not (or (= n 1) (= n (- m 1)))) 
           (= (remainder (* n n) m) 1)) 
      0
      (remainder (* n n) m)))


(define (expmod base exp m) 
  (cond ((= exp 0) 1) 
        ((even? exp) 
         (square-check (expmod base (/ exp 2) m) m)) 
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (miller-rabin-test n) 
  (define (try-it a) 
    (= (expmod a (- n 1) n) 1)) 
  (try-it (+ 2 (random (- n 1)))))

(define (fast-prime? n times) 
  (cond ((= times 0) true) 
        ((miller-rabin-test n) (fast-prime? n (- times 1))) 
        (else false)))


(fast-prime? 1009 500)
(fast-prime? 1063 500)
(fast-prime? 1129 500)
(fast-prime? 1217 500)
(fast-prime? 1579 500)





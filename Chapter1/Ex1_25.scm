(define (fast-expt b n) 
  (define (even?) 
    (= (remainder n 2) 0))
  (define (square n) 
    (* n n))
  (cond ((= n 0) 1) 
        ((even?) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))


(define (expmod base exp m) 
  (remainder (fast-expt base exp) m))

(expmod 10 40000000 11)

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


(expmod 10 40000000 11)

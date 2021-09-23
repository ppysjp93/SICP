(define (square-sum-largest2 x y z)
  (sum-of-squares (largest-of-3 x y z) 
                  (middle-of-3 x y z)))

(define sum-of-squares x y
(+ (square x) (square y)))

(define (square x)
  (* x x))

(define (largest-of-3 x y z) 
  (cond ((and (> y x) (> z y)) z)
        ((and (> y x) (> y z)) y)
        (else x)))

(largest-of-3 3 4 5)
(largest-of-3 3 5 4)
(largest-of-3 5 4 3)
(largest-of-3 5 3 4)
(largest-of-3 4 5 3)
(largest-of-3 4 3 5)

(define (smallest-of-3 x y z) 
  (cond ((and (> y x) (> z y)) x)
        ((and (> y x) (> y z)) y)
        (else x)))

; This is not hte simplest way of doing it. There must be a recursive way! 


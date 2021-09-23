(define (factorial n)
  (if (= n 1)
      n
      (* n (factorial (- n 1)))))

(factorial 4)

(define (abs x)
(cond ((> x 0) x) 
      ((= x 0) 0) 
      ((< x 0) (- x))))

(define (abs x)
(cond (( < x 0) (- x))
      (else x)))

(define (abs x)
(if (< x 0) 
    (- x) 
    x))

(abs -3)

(define (>= x y)
  (or (> x y) (= x y)))

(>= -4 -5)

(cons (list 1 2) (list 3 4))

(cdr (cons (list 1 2) (list 3 4)))


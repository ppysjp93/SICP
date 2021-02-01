(define (sqrt-iter guess x)
  (if (good-enough? guess x)
  guess
  (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y) 
 (/ (+ x y) 2))

(define (guess-good-enough? guess x) 
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x) 
  (sqrt-iter 1.0 x))

(define (abs x) 
  (if (< x 0) 
      (- x)
      x))

(define (square x) 
  (* x x))

(sqrt 9)

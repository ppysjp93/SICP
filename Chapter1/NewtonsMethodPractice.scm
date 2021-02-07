(define (sqrt-iter guess x)
(if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) 
             x)))

(define (improve guess x)
(average guess (/ x guess)))

(define (average guess x)
(/ (+ guess x) 2))

(define (good-enough? guess x)
(< (abs (- (square guess) x)) 0.0001))


(define (good-enough2? guess x)
(< (abs (- (improve guess) guess)) (/ guess 1000)))

(define (sqrt x) 
(sqrt-iter 1.0 x))

(sqrt 9)

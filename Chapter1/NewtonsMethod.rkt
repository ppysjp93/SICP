(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
(< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (abs x)
  (if (< x 0)
    (- x)
    x))

(define (square x) 
  (* x x))

(define (print x)
        x)

(sqrt 9)

; Ex1.7
; SMALL NUMBER CASE 

(sqrt 0.00000001) 

;should be 0.0001 and is 0.03125010656, this unexpected
;value is is the almost square root of the tolerance 0.001 almost 0.0316227766
;it is also the case that the unexpected value is roughly 1/32
;so the unexpected value squared is 0.000976562 which is just under the tolerance
;value of 0.001. 
;So what is the algorithm doing? 
;For small values it is essentially having 1 + delta where delta is a very small
;value close to zero. So if we approximate the algorithm it is basically having
;halving 1 and making that the guess. It keeps having 1 (5 times in total) and then
;good-enough? is satisfied eventhough it is giving the wrong answer.

; Ex1.7
; LARGE NUMBER CASE 

(sqrt 100000000000) 


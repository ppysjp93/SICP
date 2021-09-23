; Let's just collect up the blocks to build square root.
(define (square x) (* x x))

(define (improve guess x) 
  (/ (+ guess (/ x guess)) 2))

(define (good-enough? guess x) 
  (< (abs (- (square guess) x)) 0.001))

; We are trying to practice the ability to extract higher order abstractions
; We want to replace the sqrt-iter function with a iterative improve instead!

(define (sqrt-iter guess x) 
  (if (good-enough? guess x) 
      guess 
      (sqrt-iter (improve guess x) 
                 x)))

(define (sqrt x) 
  (sqrt-iter 1.0 x))

(sqrt 2)

; We want something that looks like the following so let's apply some 
; wishful thinking.

(define (sqrt x) 
  ((iterative-improve good-enough? improve) 1.0))


(define (iterative-improve good-enough? improve) 
  (lambda (x) 
   (define (iter guess) 
     (if (good-enough? guess x) 
         guess 
         (iter (improve guess x)))) 
  (iter 1.0)))

; So I think there are been a slight miss understanding I think that
; iterative improve needs to be defined more like: 

(define (iterative-improve good-enough? improve) 
  (define (iter guess) 
    (let ((next (improve guess))) 
      (if (good-enough? guess next) 
          next 
          (iter next))))
    (lambda (first-guess) (iter first-guess)))

; I think it will use a composition to get the desired result.

(define (fixed-point good-enough? improve first-guess) 
 ((iterative-improve good-enough? improve) first-guess))

; How do I resolve the fact that the improve from the sqrt takes two formal
; parameters whilst the "improve" from the fixed point is f that takes one
; parameter? why don't we just define improve as a lambda that takes a guess
; And manipulates x

(require racket/math)

(fixed-point (lambda (x y) (< (abs (- x y)) 0.00001)) 
             cos 
             1.0)

(define (square x) (* x x))

(define (average x y) (/ (+ x y) 2))

(define (sqrt x) 
  ((iterative-improve good-enough? improve) guess))

(sqrt 2)

; We are implored to review sqrt x from section 1.1.7 and to compare it with the 
; fixed point version of square root.

(define (average x y) 
  (/ (+ x y) 2))

(define (improve guess x) 
  (average guess (/ x guess)))

(define (square x) (* x x))

(define (good-enough? guess x) 
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x) 
  (if (good-enough? guess x) 
      guess 
      (sqrt-iter (improve guess x) 
                 x)))

(define (sqrt x) 
  (sqrt-iter 1.0 x))

(sqrt 10)

; The main thing I notice here is that every argument we come across for 
; every function is a primitive data type i.e. a number.
; We just have a series of compound procedures.

(define tolerance 0.00001)

(define (fixed-point f first-guess) 
  (define (close-enough? v1 v2) 
    (< (abs (- v1 v2)) tolerance))
  (define (try guess) 
    (let ((next (f guess))) 
      (if (close-enough? guess next) 
          next 
          (try next))))
    (try first-guess))

; Here we have a familiar compound procedure called close enough.
; fixed-point on the other hand is interesting because it takes 
; any general procedure f and then a primitive data type first-gues
; You can also see that we incorporate a "let" construct for defining
; the next variable.

(define (sqrt x) 
  (fixed-point (lambda (y) (/ x y)) 
               1.0))

(sqrt 2) ; This fixed point doesn't work because we need to do the damping!!!

; We now see how we can use the fixed point function to compute the square 
; root. The first thing to notice in the main is hte compactness of the code.
; We also see how a lambda expression is passed into the fixed-point which 
; is another new concept being implemented.

(define (average-damp f) 
  (lambda (x) (average x (f x)))) 

; (average is the first function defined in this script)

((average-damp square) 10)

; We now get to the new construct which is where we create a procedure 
; that takes a functional argument and then returns a function that uses 
; uses the passed in function to manipulate some value. 
; We now have a situation where the operator is not jus another symbol, it 
; is a combination itself.

(define (sqrt x) 
  (fixed-point (average-damp (lambda (y) (/ x y))) 
               1.0))

(sqrt 3)

; This is the exciting bit. We can easily make the function work for cube-root
; by adapting the fixed point. 

(define (cube-root x) 
  (fixed-point (average-damp (lambda (y) (/ x (* y y)))) 
               1.0))

(cube-root 8)

; Pretty good. 

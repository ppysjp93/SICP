; We now come to Newtons method using fixed points which is a method for finding
; the roots of any function.

; Say we want to find the root of a function g(x) = 0 
; It just so happens that if we find the fixed point of a different function
; f(x) which has the following relationship with g(x)

; f(x) = x - (/ (g(x) (D g(x)) Where D is the derivative operation.

; If you read the page 74, the most illuminating thing you should notice is that
; a derivative is just a functional transformation at its heart. 

(define (deriv g) 
 (let ((dx 0.00001)) 
   (lambda (x) (/ (- (g (+ x dx)) (g x))  
                dx))))

; I love how that idea translates so straight forwardly into scheme! 

(define (cube x) 
  ((lambda (x) (* x x x)) x)) 

; I know this is long winded but I just wanted to
; to practice using lambdas to create a procedure
; that returns a value.

((deriv cube) 5) 

; Again another example of an operator which is itself a 
; Combination

; To evaluate a comibination do the following
; Evaluate the subexpressions of the combination
; Apply the procedure that is the value of the left most subexpression 
; (the operator) to the arguments that are the other subexpressions of the 
; combination (the operands)

; This was confusing me, and I think I was right to be confused at first
; because the procedure was never a "sub-expression" that needed evaluating
; Now it is clear as day! 

(define (newton-transform g) 
 (lambda (x) (- x (/ (g x) ((deriv g) x)))))

; finally we can use the transform to create newtons-method

(define (newtons-method g guess) 
  (fixed-point (newton-transform g) guess))

; Even more explicitly

(define (newtons-method g guess) 
  (let ((f (newton-transform g))) 
    (fixed-point f guess)))

; We are now in a position to define the sqrt method one more time
(define (square x) (* x x))

(define (sqrt x) 
  (newtons-method (lambda (y) (- (square y) x))  1.0))

; We finally have a sqrt method that looks like the solution to 
; y squared = x or y squared minus x equals zero, as described on page 22.

(sqrt 2)




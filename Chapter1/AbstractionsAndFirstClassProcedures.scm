; Finally we now have the chance to take a step back and think if there is 
; a higher order function to be found.

; We have found the sqrt using a fixed point in both cases.
; In the first case we "transformed" the function we were interested in 
; by performing average damping (I wouldn't have spotted this)
; This makes sense because we defined average-damp as a procedure that 
; manipulates a general procedure and returns a new procedure in return

(define (sqrt x) 
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

; The second case (which is a more general case for finding roots of functions
; is the Newton-method. 

(define (newtons-method g guess) 
  (fixed-point (newton-transform g) guess))

(define (sqrt x) 
  (newtons-method (lambda (y) (- (square y) x))  
                  1.0))


; Notice that there is a pattern which can be abstracted

(define (fixed-point-of-transform g transform guess) 
  (fixed-point (transform g) guess))

; We can slot in average-damp or newton-transform to this abstraction
; to reformulate the square root procedure 

(define (sqrt x) 
  (fixed-point-of-transform (lambda (y) (/ x y))  
                            average-damp 
                            1.0))

(define (sqrt x) 
  (fixed-point-of-transform (lambda (y) (- (square y) x))  
                            newton-transform 
                            1.0))


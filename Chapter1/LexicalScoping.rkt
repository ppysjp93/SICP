(define (sqrt x)
  (define (square a)
    (* a a))
  (define (average a b)
    (/ (+ a b) 2))
  (define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001)) 
  (define (improve guess x)
    (average guess (/ x guess))) 
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x))) 
  (sqrt-iter 1.0 x))

(sqrt 9)

; We can now perform lexical scoping where because x is bound to the locally
; to sqrt it is avaibable to the rest of the function and doesn't need to be bound
; again for each individual defintion within the function. 

(define (sqrt x)
  (define (square a)
    (* a a))
  (define (average a b)
    (/ (+ a b) 2))
  (define (good-enough? guess)
  (< (abs (- (square guess) x)) 0.001)) 
  (define (improve guess)
    (average guess (/ x guess))) 
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess)))) 
  (sqrt-iter 1.0))

(sqrt 9)

; When performing a lexical scoping rectoring 

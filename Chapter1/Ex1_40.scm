(define (fixed-point-of-transform g transform guess) 
  (fixed-point (transform g) guess))

(define (fixed-point f first-guess) 
  (let ((tolerance 0.00001)) 
    (define (close-enough? v1 v2) 
      (< (abs (- v1 v2)) tolerance))
    (define (try guess) 
      (let ((next (f guess))) 
        (if (close-enough? guess next) 
            next 
            (try next))))
    (try first-guess)))

; To solve this question we have to use the Newton-Transform

(define (deriv g) 
  (let ((dx 0.00001)) 
    (lambda (x) (/ (- (g (+ x dx)) (g x)) 
                   dx))))

(define (newton-transform g) 
 (lambda (x) (- x (/ (g x) 
                     ((deriv g) x)))) )

; Notice the deriv is acting on the g that's passed in

(require racket/math)

(define (newtons-method g guess) 
  (fixed-point-of-transform g 
                            newton-transform 
                            guess))

(define (square x) (* x x))

(define (cube x) (* x x x))

(define (cubic a b c) 
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(newtons-method (cubic -6 11 -6) 2.3)

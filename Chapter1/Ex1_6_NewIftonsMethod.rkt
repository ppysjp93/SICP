; dont run this program, it causes an infinite recursion to call because
; new-if is a function and cond evaluates an expression list that can be 
; infinitely many items long. 
; if is better than new if for this reason as it doesn't get stuck in the infinite
; recursion trap of new-if


(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

(define ( new-if predicate then-clause else-clause) 
  (cond (predicate then-clause) 
        (else else-clause)))

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

(sqrt 9)



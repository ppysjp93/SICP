; Jut a bit of practice trying to use the mclaurin series not quite right still.

(require math)

(define (factorial x) (gamma (+ x 1)))
(factorial 3)

; We get out our higher order definition

(define (sum term a next b) 
  (if (> a b) 
      0 
      (+ (term a) 
         (sum term (next a) next b))))

(define (sine x n) 
  (define (plus-minus k) (expt -1 k))
  (define (denom k) (factorial (+ (* 2 k) 1)))
  (define (x-term k) (expt x (+ (* 2 k) 1)))
  (define (term k) (/ (* (plus-minus k) (x-term k)) (denom k)))
  (define (next k) (+ k 1))
  (sum term 0 next n))


(require racket/trace)
(trace sum)
(sine (/ pi 2) 10)



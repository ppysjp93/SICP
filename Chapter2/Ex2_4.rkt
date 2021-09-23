(define (cons x y) 
  (lambda (m) (m x y)))

(define (car z) 
  (z (lambda (p q) p)))

(define (cdr z) 
  (z (lambda (p q) q)))

(define (make-rat n d) 
  (cons n d))

(define (numer x) 
  (car x))

(define (denom x) 
  (cdr x))

(define one-half  
  (make-rat 1 2))

(numer one-half)
(denom one-half)

; This is another procedural representation of a pair. 

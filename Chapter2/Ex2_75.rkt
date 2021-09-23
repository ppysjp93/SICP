(define (square x) (* x x))

(define (make-from-real-imag r a) 
  (define (dispatch op) 
    (cond ((eq? op 'real-part) (* r (cos a))) 
          ((eq? op 'imag-part) (* r (sin a))) 
          ((eq? op 'magnitude) r) 
          ((eq? op 'angle) a) 
          (else (error "Unkown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define z1 (make-from-real-imag 1 pi))

(z1 'real-part)
(z1 'imag-part)
(z1 'magnitude)
(z1 'angle)

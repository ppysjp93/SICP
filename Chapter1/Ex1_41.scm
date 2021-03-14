(define (double f) 
  (lambda (x) (f (f x))))

(define (inc x) 
  (+ x 1))

(inc 1)

((double inc) 3)

(((double double) inc) 3)

(inc (inc (inc (inc 3))))

(((double (double double)) inc) 5)

; I'm expecting 13
; Instead it was 21
 





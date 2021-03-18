(define (print-rat x) 
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (abs x)
(if (< x 0) 
    (- x) 
    x))

(define (make-rat n d) 
 (let ((g (gcd n d))) 
   (if (< d 0) 
       (cons (/ (* n -1) g) (/ (* d -1) g)) 
       (cons (/ n g) (/ d g)))))

(define one-third (make-rat -1 -3))
(define -one-third (make-rat -1 3))
(define -one-third2 (make-rat 1 -3))
(define one-third2 (make-rat 1 3))

(print-rat one-third)
(print-rat -one-third)
(print-rat -one-third2)
(print-rat one-third2)










(define (make-rat n d) (cons n d))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x) 
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

; Notice print-rat has to take a pair as the data structure for it to work

(define one-half (make-rat 1 2))

(print-rat one-half)

(define one-third (make-rat 1 3))


(define (add-rat x y) 
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y) 
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(print-rat (add-rat one-half one-third))

(print-rat (mul-rat one-half one-third))

(print-rat (add-rat one-third one-third))


(define (gcd x y) 
  (if (= y 0) 
      x
      (gcd y (remainder x y))))

(define (make-rat n d) 
  (cons (/ n (gcd n d))
        (/ d (gcd n d))))


; We can upgrade this further by using a let :)

(define (make-rat n d) 
 (let ((g (gcd n d))) 
   (cons (/ n g) (/ d g))))


(define one-third (make-rat 1 3))

(print-rat one-half)

(define (normalise n d) 
  (cond ((< d 0) (* d -1)) 
        ((= b 2) 2) 
        (else 3)))

(define (abs x)
(if (< x 0) 
    (- x) 
    x))









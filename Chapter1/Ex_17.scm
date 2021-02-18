; this is a linear recursive algorithm so not the best but a good place to
; start looking for alternatives 

(define (times a b) 
  (if (= b 0) 
    0
    (+ a (times a (- b 1)))))

(times 3 4)

(define (double a) 
  (+ a a))

(define (halve b) 
  (/ b 2))

(define (even?? n) 
  (= (remainder n 2) 0))

(define (times a b) 
  (cond ((= b 0) 0) 
        ((even?? b) (times (double a) (halve b))) 
        (else (+ a (times a (- b 1))))))

(times 4 3)

(times 4 10)

; this is a logarithmic recursive algorithm which is a vast improvement.

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

; Here is the iterative definition


(define (times a b) 
  (times-iter a b 0))

(define (times-iter a b c) 
  (cond ((= b 0) c) 
        ((even?? b) (times-iter (* 2 a) 
                                (/ b 2) 
                                c)) 
        (else (times-iter a (- b 1) (+ a c)))))

(times 4 10)
(times 10 13)

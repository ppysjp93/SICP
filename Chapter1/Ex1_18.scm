(define (times a b) 
  (define (times-iter a b c) 
    (cond ((= b 0) c) 
          ((even? b) (times-iter (* 2 a) (/ b 2) c)) 
          (else (times-iter a (- b 1) (+ a c)))))
  (times-iter a b 0))

(times 3 4)




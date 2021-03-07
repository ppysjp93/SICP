; Here n stands for numerator and d stand for denomiator and k is the depth
; of the contuous fraction

(define (cont-frac n d k) 
 (if (= k 1) 
     (/ n d) 
     (/ n (+ d (cont-frac n d (- k 1))))))

(cont-frac 1.0 1.0 11)

; To four decimal places the inverse golden ratio is 0.6180340
; It takes a depth of 11 to get continued fraction to get the same result

; template 

(define (times a b) 
  (define (times-iter a b c) 
    (cond ((= b 0) c) 
          ((even? b) (times-iter (* 2 a) (/ b 2) c)) 
          (else (times-iter a (- b 1) (+ a c)))))
  (times-iter a b 0))

(times 3 4)

; Solution

(define (cont-frac n d k) 
  (define  (cont-frac-iter n d k f)
    (define (next x) x)
    (define (combine x y z) (/ x (+ d f)))
    (if (= k 1) 
          f 
          (cont-frac-iter (next n) 
                          (next d) 
                          (- k 1) 
                          (combine n d f))))
  (cont-frac-iter n d k (/ n d)))

(cont-frac 1.0 1.0 11)


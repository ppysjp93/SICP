(define (cont-frac n d k) 
 (if (= k 1) 
     (/ n d) 
     (/ n (+ d (cont-frac n d (- k 1))))))


(define (cont-frac n d k term next) 
 (if (= k 1) 
     (/ n d) 
     (term n d (cont-frac n d (next k) term next))))


(define (term x y z) (/ x (+ y z)))
(define (next x) (- x 1))

(cont-frac 1.0 1.0 10 term next)

; now we can convert it to an iterable

(define (cont-frac n d k term next) 
  (define (iter n d k result) 
   (if (= k 1) 
       result 
       (iter n d (next k) (term n d result))))
  (iter n d k (/ n d)))


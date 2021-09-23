(define (term x) 
  (let ((y (/ (+ x 1) 3))
        (z (ceiling (/ x 3))))   
  (if (= y z) 
          (* z 2) 
          1)))

(define  (cont-frac-iter n d k f)
  (define (nextn x) x)
  (define (nextk x) (- x 1))
  (define (combine x y z) (/ x (+ y z)))  
  (if (= k 1) 
        f 
        (cont-frac-iter (nextn n) 
                        (term (nextk k)) 
                        (nextk k) 
                        (combine n (term (nextk k)) f))))


(define (cont-frac n d k) 
 (cont-frac-iter n d k (/ n d)))

(require racket/trace)
(trace cont-frac-iter)
(cont-frac-iter 1 (term 5) 5 (/ 1.0 (term 5)))

(trace cont-frac)

(define (e k) 
 (+ 2 (cont-frac-iter 1 (term k) k (/ 1.0 (term k)))))

(e 100)












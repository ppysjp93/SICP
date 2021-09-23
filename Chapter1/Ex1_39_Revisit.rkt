; Since I spent so much time making my iterative form of cont-frac I'm going
; to use it

(define (cont-frac-iter n d next combine k) 
  (define (iter i result) 
    (if (= i 1) 
        result 
        (iter (next i) (combine (n (next i)) (d (next i)) result))))
  (iter k (/ (n k) (d k))))

(define (cont-frac n d k) 
  (cont-frac-iter n 
                  d 
                  (lambda (i) (- i 1))  
                  (lambda (n d f) (/ n (- d f))) 
                  k))


(define (tan-cf x k) 
  (/ (cont-frac (lambda (i) (* x x)) 
                (lambda (i) (- (* 2 i) 1))
                k) x))

(require racket/trace)
(trace cont-frac-iter)
(tan-cf (/ pi 4) 10)

(require math)

(define  (cont-frac-iter n d k f inc term-n term-d combine)
  (let ((next-n n)
        (next-d (term-d (inc k))) 
        (next-k (inc k))) 
   (let ((next-f (combine next-n next-d f))) 
    (if (= k 1) 
            f 
            (cont-frac-iter next-n next-d next-k next-f 
                            inc term-n term-d combine)))))

(define (tan-cf x k) 
  (define (inc x) (- x 1))
  (define (term-n x) (* x x))
  (define (term-d x) (- (* 2 x) 1))
  (define (combine x y z) (/ x (- y z))) 
  (/ (cont-frac-iter (term-n x) (term-d k) k (/ (term-n x) (term-d k)) 
  inc term-n term-d combine) x))


(require racket/trace)
(trace cont-frac-iter)
(tan-cf (/ pi 4) 10)

; tan pi/4 is 1 by the way so that's great :)

















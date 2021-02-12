(define (nlessthan3 n) 
  (cond ((= n 0) 0)
        ((= n 1) 1)
        ((= n 2) 2)))

(nlessthan3 0)
(nlessthan3 1)
(nlessthan3 2)



(define (f n) 
  (if (< n 3) 
      (nlessthan3 n)
      (+ (f (- n 1)) 
         (* 2 (f (- n 2))) 
         (* 3 (f (- n 3))))))


(f 0)
(f 1)
(f 2)
(f 3)
(f 4)
(f 5)
(f 6)
(f 7)

(define (f-iter counter a b c d n) 
 (if (= (- n counter) 2)
     d
     (f-iter 
            (+ counter 1)
            d 
            (* 2 a)
            (/ (* 3 b) 2)
            (+ d (* 2 a) (/ (* 3 b) 2))
            n)))

(f-iter 0 1 0 0 2 n)

(define (f n) 
  (define (f-iter counter a b c d n) 
   (if (= (- n counter) 2)
       d
       (f-iter 
              (+ counter 1)
              d 
              (* 2 a)
              (/ (* 3 b) 2)
              (+ d (* 2 a) (/ (* 3 b) 2))
              n)))
  (if (< n 2)
    n
    (f-iter 0 1 0 0 2 n)))

; Think about how you could perform a tiny bit of lexical scoping to tidy up
; the above iterator

(f 0)
(f 1)
(f 2)
(f 3)
(f 4)
(f 5)
(f 6)
(f 7)





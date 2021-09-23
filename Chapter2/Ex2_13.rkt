(define (mul-interval x y) 
  (make-centre-percent (* (centre x) (centre y)) 
                       (+ (percentage x) (percentage y))))

; so now we can test out using our two different mul-interval methods
; use ripgrep to find the other mul-interval definition for loading.

(define c (mul-interval a b))

(print-interval a)
(print-interval b)
(print-interval c)

; the upper bound of c is 6.8*4.7*1.15
 

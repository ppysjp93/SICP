(define (Pascal n r) 
  (if (or (= r 0) (= n r)) 
       1
       (+ (Pascal (- n 1) (- r 1)) 
          (Pascal (- n 1) r))))


(Pascal 4 2)

; A more efficient way of implementing Pascal is to use probably
; to use the iterative formula for factorial and then use the modern
; formula which is n!/(r!*(n-r)!)

(/ 101 3.)
(floor (/ 101 3))


; To add-intevals is straight-forward enough.

(define (add-interval x y) 
  (make-interval (+ (lower-bound x) (lower-bound y)) 
                 (+ (upper-bound x) (upper-bound y))))

; Think of these points on a set of axes is probably a good way of 
; thinking of it. We basically select the two points that are 
; furthest from zero and say that they are what we want.

(define (mul-interval x y) 
  (let ((p1 (* (lower-bound x) (lower-bound y))) 
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y)))) 
    (make-interval (min p1 p2 p3 p4) 
                   (max p1 p2 p3 p4))))


; To do division multiply the first interval by the recipricol of the second.

(define (div-interval x y) 
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y)))
                (make-interval (/ 1.0 (lower-bound y)))))

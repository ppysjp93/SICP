(define a (make-centre-percent 1 0.001))
(define b (make-centre-percent 1 0.005))

(centre (div-interval a a))
(percentage (div-interval a a))
(centre (div-interval a b))
(percentage (div-interval a b))

; When the width is a small percentage of the central value we no longer
; get any addition of the percentages instead we just get zero as the numbers 
; are too small to evaluate one presumes. 

; In the par2 formula we have widths of zero because of the use of the one 
; intervals. This has the effect of reducing the combined percentage of the 
; parallel resistor in the end. 
; par2 on the other hand suffers from the fact that the interval produced 
; by R1*R2 produces a relatively large percentage. This percentage is 
; then combined to make an even bigger percentage in the numerator. 



; Alyssa reasons for adding two intervals the following.
; She reasons that the minimum value the sum could be is the sum of the 
; two lower bounds and the maximum value it could be is the sum of the upper
; bounds. 

;The smallest value possible when subtracting interval y from interval x is to 
;subtract the upper bound of y from the lower bound of x. The largest result of 
;subtracting interval y from interval x is to subtract the lower bound of y from the 
;upper bound of x.

; [x1, x2] - [y1, y2] = [x1 - y2, x2 - y1] 

; lower: this is because x1 and y2 are the furthest from each other
; upper: this is because x2 and y1 are teh closest to each other. 

(define (sub-interval x y) 
  (make-interval (- (lower-bound x) (upper-bound y)) 
                 (- (upper-bound x) (lower-bound y))))

(define (print-interval interval) 
  (newline) 
  (display "[") 
  (display (lower-bound interval)) 
  (display ",") 
  (display (upper-bound interval)) 
  (display "]"))

(define a (make-interval 1 4))

(print-interval a)

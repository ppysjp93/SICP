(define (make-centre-percent centre percentage) 
  (make-interval (* centre (- 1 (/ percentage 100.0))) 
                 (* centre (+ 1 (/ percentage 100.0)))))

(define (print-interval i) 
  (newline) 
  (display "[") 
  (display (lower-bound i)) 
  (display ",") 
  (display (upper-bound i)) 
  (display "]"))

; though this is technically correct we are given the following from the 
; textbook 

(define (make-centre-width c w) 
  (make-interval (- c w) (+ c w)))

(define (centre i) 
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i) 
  (/ (- (upper-bound i) (lower-bound i)) 2))

; So another way to define an interval using make-centre-percent would be the 
; following

(define (make-centre-percent centre percentage) 
  (make-centre-width centre (* centre (/ percentage 100))))

; we need a way of selecting the percentage from the interval

(define (percentage i) 
  (* 100 (/ (width i) (centre i))))

(define a (make-centre-percent 6.8 10))
(define b (make-centre-percent 4.7 5))

(print-interval a)
(print-interval b)

(percentage a)





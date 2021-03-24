; We start by defining the constructors and selectors for the top level

(define (make-segment p1 p2) 
  (cons p1 p2))

(define (start-segment s) 
  (car s))

(define (end-segment s) 
  (cdr s))

; Next we define the constructors and selectors for the next level.

(define (make-point x y) 
  (cons x y))

(define (x-point p) 
  (car p))

(define (y-point p) 
  (cdr p))

; x-point and y-point are the lowest level because they are primitives
; so now we turn our focus to creating midpoint-segment

(define (midpoint-segment s) 
  (average-point (start-segment s)
                 (end-segment s)))

; Notice how we have kept the levels separate and we are using procedures to
; to act as intermediaries between the levels of the system that's being 
; created. We need to now define what average point actually does.

(define (average-point p1 p2) 
  (make-point (average (x-point p1) (x-point p2))
              (average (y-point p1) (y-point p2))))

; Now we just need to define how average will act on the primitive data types

(define (average x y) (/ (+ x y) 2))

; So in theory we should be ready to test our solution out with some
; concretions. First lets create 2 points.

(define p1 (make-point 1 1))
(define p2 (make-point 5 5))

; Let's now create the segment and find its midpoint. 

(define s1 (make-segment p1 p2))
(define midpoint-s1  (midpoint-segment s1))

; Finally we can use the print point function which is given to print the 
; position of the midpoint of the segment.

(define (print-point p) 
  (newline) 
  (display "(") 
  (display (x-point p)) 
  (display ",") 
  (display (y-point p)) 
  (display ")"))

(print-point midpoint-s1)

; Low and behold we now have a very nice solution to the problem




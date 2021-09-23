(define (make-vect xcor ycor) 
  (cons xcor ycor))

(define (xcor-vect v) 
  (car v))

(define (ycor-vect v) 
  (cdr v))

(define (add-vect v1 v2) 
  (make-vect (+ (xcor-vect v1) 
                (xcor-vect v2)) 
             (+ (ycor-vect v1) 
                (ycor-vect v2))))

(define (sub-vect v1 v2) 
  (make-vect (- (xcor-vect v1) 
                (xcor-vect v2)) 
             (- (ycor-vect v1) 
                (ycor-vect v2))))

(define (scale-vect s v) 
  (make-vect (* s (xcor-vect v)) 
             (* s (ycor-vect v))))

; Solution starts here, a and b are the 2 vectors mentioned in the question
; see notes for reasoning. 

(define (make-segment a b) 
  (cons a b))

(define (start-segment segment) 
  (car segment))

(define (end-segment segment) 
  (cdr segment))

(define a (make-vect 2 4))

(define b (make-vect 7 5))

(make-segment a b)



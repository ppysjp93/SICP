(require math)

(define (average a b) (/ (+ a b) 2))

(define (close-enough? x y) 
  (< (abs (- x y)) 0.001))

; Notice two things here. First, the use of let, is exactly, how you define 
; variables when you are doing your own maths. So that is a nice parallel. 
; Secondly there are nested 'let' statements but they allow for a very natural
; flowing function. It just reads and makes a lot of sense as a set of 
; instructions.

(define (search f neg-point pos-point) 
  (let ((midpoint (average neg-point pos-point))) 
    (if (close-enough? neg-point pos-point) 
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))


(define (half-interval-method f a b) 
  (let ((a-value (f a)) 
        (b-value (f b))) 
    (cond ((and (negative? a-value) (positive? b-value)) 
           (search f a b)) 
          ((and (negative? b-value) (positive? a-value)) 
           (search f b a)) 
          (else 
            (error "Values are not of opposite sign" a b)))))

(half-interval-method sin 2.0 4.0)

(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))  
                      1.0 2.0)

; Fixed point 
(define tolerance 0.00001)

(define (fixed-point f first-guess) 
  (define (close-enough? v1 v2) 
    (< (abs (- v1 v2)) tolerance)) 
  (define (try guess) 
    (let ((next (f guess))) 
      (if (close-enough? guess next) 
          next 
          (try next)))) 
  (try first-guess))

(fixed-point cos 1.0)

; We can use use fixed point on more complex functions as well if we like


(fixed-point (lambda (y) (+ (sin y) (cos y))) 
             1.0)

; We can define the square root as a fixed point

(define (sqrt1 x) 
  (fixed-point (lambda (y) (/ x y)) 
               1.0))

; Unfortunately this is an inifinite loop, if you try it with some variables on
; some paper it makes sense
; Fortunately we can upgrade the function and make it work by forcing the 
; change on each iteration to be constrained to a smaller value.

(define (sqrt2 x) 
  (fixed-point (lambda (y) (average y (/ x y))) 
               1.0))

(sqrt2 2)

; Apperently if we unravel this fixed-point abstraction we will get back to our 
; original square root function. This might be an interesting exercise to do 
; at some point. 

; Ex1_35

; Finding the golden ratio

(/ (+ 1 (sqrt2 5)) 2)

(define (golden-ratio y) 
  (fixed-point (lambda (y) (+ 1 (/ 1 y))) 
               1.0))

(golden-ratio 3)
















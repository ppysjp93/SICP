(define (square a)
    (* a a))

(define (cube a) 
  (* a a a))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (improve guess x)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cbrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (cbrt-iter (improve guess x) x)))

(define (cbrt x)  
  (cbrt-iter 1.0 x))

(cbrt 27)

; So the standard good enough isn't working because I forgot to use the 
; cube instead of the square for testing. I now change good enough so that 
; uses good-enough with a cube. 
; Now that I have the deifinitions for cube root set up I can now use a block 
; structure 


(define (cbrt x) 
  (define (square a)
    (* a a))
  (define (cube a) 
    (* a a a))
  (define (good-enough? guess x)
    (< (abs (- (cube guess) x)) 0.001))
  (define (improve guess x)
      (/ (+ (/ x (square guess)) (* 2 guess)) 3))
  (define (cbrt-iter guess x)
      (if (good-enough? guess x)
          guess
          (cbrt-iter (improve guess x) x)))
  (cbrt-iter 1.0 x))

(cbrt 27)

; finally I perform lexical scoping to refactor the code even further

(define (cbrt x) 
  (define (square a)
    (* a a))
  (define (cube a) 
    (* a a a))
  (define (good-enough? guess)
    (< (abs (- (cube guess) x)) 0.001))
  (define (improve guess)
      (/ (+ (/ x (square guess)) (* 2 guess)) 3))
  (define (cbrt-iter guess)
      (if (good-enough? guess)
          guess
          (cbrt-iter (improve guess))))
  (cbrt-iter 1.0))

(cbrt 27)

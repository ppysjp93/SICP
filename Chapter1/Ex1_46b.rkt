(define (compose f g) 
  (lambda (x) (f (g x))))

(define (repeated f n) 
  (if (= n 1) 
      f 
      (compose f (repeated f (- n 1)))))

(define (repeated f n) 
  (define (iter i result) 
    (if (= i n) 
        result 
        (iter (+ i 1) (compose result f))))
  (iter 1 f))

((repeated cos 2) 1.0) 

(cos (cos 1.0))

; Building Blocks above for creating the iterative-improve

(define (iterative-improve good-enough? improve) 
 (define (iter guess i) 
   (if ((good-enough? improve) guess) 
       guess 
       (iter ((repeated improve i) guess) (+ i 1)))) 
 (lambda (guess) (iter guess 1)))

; A major step in the solution to the above was spotting that good-enough? was
; a procudure that takes a procedure as argument and returns a procedure.
; Another key step was to use the very powerful procedure "repeated" within
; the iterator so that we can perform repeated improvements. It also reads very
; well.

; Fixed Point Solution

(define (fixed-point f first-guess) 
  (define improve f) 
  (define (good-enough? improve) 
    (lambda (x) (< (abs (- x (improve x))) 0.00001)))
  ((iterative-improve good-enough? improve) first-guess))

(fixed-point cos 1.0)

; Sqrt Solution

; We want the "improve" procedure to take one primitive formal argument
; And it return a procedure that also takes one primitive formal argument.
; We want to create a procedure for "good-enough?" that takes one primitive
; formal argument and returns a procedure.

(define (iterative-improve good-enough? improve) 
 (define (iter guess i) 
   (if ((good-enough? improve) guess) 
       guess 
       (iter ((repeated improve i) guess) (+ i 1)))) 
 (lambda (guess) (iter guess 1)))

(define (square x) (* x x))

(define (average x y) (/ (+ x y) 2))

(define (sqrt x) 
  (define (improve guess) 
    ((lambda (guess) (average guess (/ x guess))) guess))
  (define (good-enough? improve) 
    (lambda (guess) (< (abs (- (square (improve guess)) x)) 0.001)))
  ((iterative-improve good-enough? improve) 1.0))

(sqrt 2)


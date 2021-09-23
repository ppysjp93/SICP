(require racket/math)

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

(define (average x y) (/ (+ x y) 2))

(define (average-damp f) 
  (lambda (x) (average x (f x))))

(define (square x) (* x x))

(define (cube-root x) 
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 
               1.0))

(cube-root 8)

; Solution

; So now we have a method for finding the 4th-root

(define (4th-root x) 
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (expt y 3)))) 
               1.0))

(4th-root 1000)

; We are then asked to find a pattern for the number of average damps 
; required when finding the nth root

(define (nth-root x n) 
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (expt y (- n 1))))) 
             1.0))

(nth-root 1000 5)

(nth-root 1000 6)

(nth-root 1000 7)



(define (nth-root x n) 
  (fixed-point ((repeated average-damp 3) (lambda (y) (/ x (expt y (- n 1))))) 
             1.0))

(nth-root 1000 8)

(nth-root 1000 9)

(nth-root 1000 10)

(nth-root 1000 11)

(nth-root 1000 12)

(nth-root 1000 13)

(nth-root 1000 14)

(nth-root 1000 15)

(nth-root 1000 16)

; So it looks as though we need (floor (/ (log n) (log 2))) damps  
; We just have the defined the log base b in terms of log in the floor 
; procedure i.e. (log-2 n).

(define (log-2 x) (/ (log x) (log 2)))

(define (nth-root x n) 
(let ((number-of-damps (floor (log-2 n))))
  (if (= n 1) 
      x
      (fixed-point ((repeated average-damp number-of-damps) 
                   (lambda (y) (/ x (expt y (- n 1))))) 
                   1.0))))


(nth-root 1000 1)

(nth-root 1000 16)









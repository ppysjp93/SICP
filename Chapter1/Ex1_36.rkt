(require racket/math)

; I'm asked to upgrade the fixed-point so it does a newline and displays
; each guess to the repl

(define tolerance 0.00001)

(define (fixed-point f first-guess) 
    (define (close-enough? v1 v2) 
      (< (abs (- v1 v2)) tolerance))
    (define (try guess) 
      (let ((next (f guess))) 
        (newline)
        (display next)
        (if (close-enough? guess next) 
            (and (newline) guess)  
            (try next))))
    (try first-guess))

(fixed-point cos 1.0)

(lambda (x) (/ (log 1000) (log x)))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 100)

(define (average x y) (/ (+ x y) 2))

(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2)



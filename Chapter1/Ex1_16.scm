(define (exponent b n) 
  (exponent-iter 1 b n))

(define (square x) 
  (* x x))

(define (exponent-iter a b n) 
  (cond ((= n 1) (* a b)) 
        ((even? n) (exponent-iter a (square b) (/ n 2)) ) 
        (else (exponent-iter (* a (/ a b)) b (+ n 1)))))

; have you done the condition statement correctly? No 

; This is an iterative-logarthimic design for finding exponentials
; It is truly glorious because it takes very few steps and is space 
; efficient! 


(exponent 3 1000000)

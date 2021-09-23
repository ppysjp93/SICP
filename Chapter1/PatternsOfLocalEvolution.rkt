; The first pattern we are going to look at is a linear recursive process
; demonstrated by the factorial function.

(define (factorial n) 
  (if (= n 0)
    1  
    (* n (factorial (- n 1)))))

(factorial 3)

; The second pattern we are going to look at is a 'linear iterative process'

(define (factorial n) 
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count) 
  (if (< max-count counter)
    product
    (fact-iter (* product counter) 
               (+ counter 1) 
               max-count)))

(factorial 4)

; max-count isn't in brackets because it is a value straight up.
; putting parentheses around it is the same as the issues we faced in 
; exercise 1.5, when we found because scheme is using the substituional 
; model for calculations then it can't evaluate (p) where p is a primitive of
; some sort

; Let's now take this opportunity to perform some lexical scoping on our 
; iterative factorial procedure

(define (factorial n) 
  (define (fact-iter product counter) 
    (if (< n counter)
      product
      (fact-iter (* product counter) 
                 (+ counter 1))))
  (fact-iter 1 1))

(factorial 3)

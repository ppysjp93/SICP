; So we can construct data using procedures. The "data" in this case is the 
; product 2^a * 3^c which we want to represent as a pair
; we know the condition for what a pair is and we know the names of the 
; constructors for creating the pair so let's do it. 

(require racket/math)

(define (cons a b) 
  (* (expt 2 a) (expt 3 b)))

; the difficult thing here is representing car and cdr because need to 
; work out a way of separting the integers that are together.
; Thanks to the fundamental theorem of arithmetic, any integer values 
; that are inserted for the values of a and b, will then result in a unique
; pair datastructure.

; If we define a general function that takes an integer and a dividor that 
; then returns the number of times that integer is divisible by then this is 
; a good step to solving the problem.

(define (number-of-divisions x d) 
  (define (iter x result) 
    (if (not (= (remainder x d) 0))
        result
        (iter (/ x d) (+ result 1))))
  (if (= d 1) 
      (error 
        "An integer divided by 1 is itself and thus causes an infinite loop." ) 
      (iter x 0)))


(define (car x) 
 (number-of-divisions x 2))

(define (cdr x) 
 (number-of-divisions x 3))

(cons 2 3)
(car (cons 2 3))
(cdr (cons 2 3))

; Thus we have an arithmetic representation for a pair as it meets the conditions
; that the constructor and the selectors must have with each other. 



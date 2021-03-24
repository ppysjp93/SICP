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



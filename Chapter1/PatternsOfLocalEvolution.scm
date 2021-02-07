; The first pattern we are going to look at is a linear recursive process
; demonstrated by the factorial function.

(define (factorial n) 
  (if (= n 0)
    1  
    (* n (factorial (- n 1)))))

(factorial 3)

; The second pattern we are going to look at is a 'linear iterative process'



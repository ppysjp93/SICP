; This is the higher order cont-frac function that I have made. 
; It is iterative in form so it is more space efficient.

(define  (cont-frac-iter n d k f inc term-n term-d)
  (let ((next-n (term-n n))
        (next-d (term-d (inc k))) 
        (next-k (inc k))) 
   (let ((next-f (/ next-n (+ next-d f)))) 
    (if (= k 1) 
            f 
            (cont-frac-iter next-n next-d next-k next-f 
                            inc term-n term-d)))))

; because this runs from the bottom of the truncated function the inputs
; have to be acted upon by the function definitions that you input into the 
; main body of the function. 

(define (e-approx k) 
  (define (inc x) (- x 1))
  (define (term-n x) x)
  (define (term-d x) 
    (let ((y (/ (+ x 1) 3))
          (z (ceiling (/ x 3))))   
    (if (= y z) 
            (* z 2) 
            1)))
  (+ 2 (cont-frac-iter (term-n 1.0) (term-d k) k (/ (term-n 1.0) (term-d k)) 
  inc term-n term-d)))

(e-approx 100)

; That was challenging. I think it's important to make sure from now on that 
; wherever we can that we keep the formal parameters of a main function separate
; from the inner function definitions.

; Maybe one day when I come back to this file I'll have the skills to fix this
; so that it works without having to do the "strange" initalizations for 
; so that term-n and term-d are used inside the iterator and not as arguments.


















; a and b are 6.8 and 4.7 ohm respectively. We get the correct answer
; which is alluded to in the book. 

(define (par1 x y) 
  (div-interval (mul-interval x y) (add-interval x y)))

(par1 a b)

(define (par2 x y) 
  (let ((one (make-interval 1 1))) 
    (div-interval one 
                  (add-interval (div-interval one x) 
                                (div-interval one y)))))

(par2 a b)
 
; par1 gives the wrong answer

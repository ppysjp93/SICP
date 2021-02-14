(define (cube x) 
  (* x x x))

(define (p x) 
  (- (* 3 x) 
     (* 4 (cube x))))

(define (sine theta) 
  (if (not (> (abs theta) 0.1))
    theta
    (p (sine (/ theta 3.0)))))


(abs -3)
(cube -3)
(sine 12.5)
(remainder 3 2)

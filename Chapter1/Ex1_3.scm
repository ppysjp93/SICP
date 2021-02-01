(define (square x) (* x x))

(square 4)

(define (sum-of-squares x y)
  ( + (square x) (square y)))

(sum-of-squares 3 4)

(define (>= x y)
    (not (< x y)))

(>= 3 4)

(>= 4 4)

(>= 5 4)

(define (square-sum-biggest-two x y z) 
    (cond ((and (>= x y) (>= y z)) (sum-of-squares x y))
          ((and (>= x y) (>= z y)) (sum-of-squares x z))
          ((and (>= y z) (>= z x)) (sum-of-squares y z))
          ((and (>= y z) (>= x z)) (sum-of-squares y x))
          ((and (>= z x) (>= x y)) (sum-of-squares z x))
          ((and (>= z x) (>= y x)) (sum-of-squares z y))
          ))

(square-sum-biggest-two 5 4 3 ) 
(square-sum-biggest-two 5 3 4 ) 
(square-sum-biggest-two 3 5 4 ) 
(square-sum-biggest-two 4 5 3 ) 
(square-sum-biggest-two 3 4 5 ) 
(square-sum-biggest-two 4 3 5 ) 

(define (p) (p))

(define (test x y)
(if (= x 0)
    0
    y))

(test 0 (p))

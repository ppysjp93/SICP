(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))

(car (car z))

; the first element of the first element of z i.e. the first element of x

(car (cdr z))

; the first element of the second element of z i.e. the first element of y


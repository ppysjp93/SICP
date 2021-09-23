(define (div-interval x y) 
  (define (interval-span-zero? y) 
    (and (< (lower-bound y) 0) 
         (> (upper-bound y) 0)))
(if (interval-span-zero? y) 
        (error "denominator interval cannot span zero")
        (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))

(define x (make-interval 1 2))
(define y (make-interval -10 10))
(print-interval (div-interval x y))

(define y (make-interval 10 15))
(print-interval (div-interval x y))


(define (parallel-restance x y) 
  (let ((one (make-interval 1 1))) 
    (div-interval one 
                  (add-interval (div-interval one x) 
                                (div-interval y)))))

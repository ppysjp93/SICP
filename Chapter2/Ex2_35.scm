(define (count-leaves t) 
  (accumulate + 
              0 
              (map (lambda (x) x) (enumerate-tree t))))

(define x (list (list 1 2) (list 3 4) 5))

(count-leaves (list x))

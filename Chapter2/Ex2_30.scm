(define x (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(define (square-tree tree) 
  (cond ((null? tree) nil) 
        ((not (pair? tree)) (* tree tree)) 
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(square-tree x)

(define (square-tree tree) 
  (map (lambda (sub-tree) 
         (if (pair? sub-tree) 
             (square-tree sub-tree) 
             (* sub-tree sub-tree)))  
       tree))

(square-tree x)

; So cool, that map has the power to do this. 


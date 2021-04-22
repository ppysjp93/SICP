(define x (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(define (tree-map proc tree) 
  (cond ((null? tree) nil) 
        ((not (pair? tree)) (proc tree)) 
        (else (cons (tree-map  proc (car tree))
                    (tree-map proc (cdr tree))))))

(define (square-tree tree) 
  (tree-map (lambda (x) (* x x)) x))

(square-tree x)

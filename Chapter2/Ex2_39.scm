(define (reverse sequence) 
  (fold-right (lambda (x y) (append (list x) y)) nil sequence))

(reverse (list 1 2 3))

(define (reverse sequence) 
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(reverse (list 1 2 3))


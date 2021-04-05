(define x (cons (list 1 2) (list 3 4)))

(list x x)

(length (list x x))

(define (count-leaves x) 
  (cond ((null? x) 0) 
        ((not (pair? x)) 1) 
        (else (+ (count-leaves (car x)) 
                 (count-leaves (cdr x))))))

(count-leaves x)

(list 1 (list 2 (list 3 4)))

(list 1 4)

(cons 1 (cons 4 nil))


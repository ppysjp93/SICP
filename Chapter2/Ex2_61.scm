(define (element-of-set? x set) 
  (cond ((null? set) false) 
        ((= x (car set)) true) 
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set) 
  (cond ((null? set) (list x)) 
        ((= x (car set)) set)
        ((and (< x (car set)) (null? (cdr set))) (cons x (car set))) 
        ((and (> x (car set)) (null? (cdr set))) (cons (car set) x))
        ((and (> x (car set)) (< x (cadr set))) 
         (cons (car set) 
               (cons x 
                     (cons (cadr set) 
                           (cddr set)))))
        ((> x (car set)) (cons (car set) (adjoin-set x (cdr set))))))

; Tests 

(adjoin-set 2 null)

(adjoin-set 4 (list 5))

(adjoin-set 6 (list 5))

(adjoin-set 2 (list 1 3 5))

(adjoin-set 4 (list 1 3 5))

(adjoin-set 3 (list 1 3 5))










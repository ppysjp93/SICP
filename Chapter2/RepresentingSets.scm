(define (element-of-set? x set) 
  (cond ((null? set) false) 
        ((equal? x (car set)) true) 
        (else (element-of-set? x (cdr set)))))


(element-of-set? 3 (list 1 2 3 4))

(element-of-set? 5 (list 1 2 3 4))


(define (adjoin-set x set) 
  (if (element-of-set? x set) 
      set 
      (cons x set)))

(adjoin-set 5 (list 1 2 3 4))

(define (intersection-set set1 set2) 
  (cond ((or (null? set1) (null? set2)) null) 
        ((element-of-set? (car set1) set2) 
         (cons (car set1) (intersection-set (cdr set1) set2))) 
        (else (intersection-set (cdr set1) set2))))


(intersection-set (list 1 2 3) (list 3 4 5))

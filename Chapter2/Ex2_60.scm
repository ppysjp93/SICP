(element-of-set? 3 (list 2 3 2 1 3 2 2))

(element-of-set? 4 (list 2 3 2 1 3 2 2))

; Element of set is the same

(adjoin-set 3 (list 2 3 2 1 3 2 2))

(adjoin-set 4 (list 2 3 2 1 3 2 2))

(define (adjoin-set x set) 
  (cons x set))

(intersection-set (list 1 2 3) (list 1 2 3))

(intersection-set (list 1 2 3) (list 2 3 2 1 3 2 2))

(union-set (list 1 2 3) (list 1 2 3 4))

(union-set (list 1 2 3) (list 2 3 2 1 3 2 2))

(define (union-set set1 set2) 
  (append set1 set2))


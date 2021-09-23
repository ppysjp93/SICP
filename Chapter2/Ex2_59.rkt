(define (union-set set1 set2) 
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (adjoin-set (car set1) 
                          (union-set (cdr set1) set2)))))

; above isn't as effiecient as it could be so we need to refactor

(define (union-set set1 set2) 
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2) 
         (union-set (cdr set1) set2)) 
        (else (cons (car set1) 
                    (union-set (cdr set1) set2)))))

(union-set (list 1 2 3) (list 3 4 5 6))

; The union-set computers the union of two sets, which is the set containing
; each elemnt in either argument



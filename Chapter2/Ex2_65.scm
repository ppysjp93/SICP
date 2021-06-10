; 1) Convert balanced binary trees to ordered lists
; 2) Perform the union-set or interection-set (see page 155)
; 3) Convert the result back to partial tree

(define (intersection-list-set set1 set2) 
  (if (or (null? set1) (null? set2)) 
      '() 
      (let ((x1 (car set1)) (x2 (car set2))) 
        (cond ((= x1 x2) (cons x1 
                               (intersection-set (cdr set1) 
                                                 (cdr set2)))) 
              ((< x1 x2) (intersection-set (cdr set1) set2)) 
              ((< x2 x1) (intersection-set set1 (cdr set2)))))))

(define (union-list-set set1 set2) 
  (cond ((and (null? set1) (null? set2)) null) 
      ((null? set1) set2) 
      ((null? set2) set1)  
      (else 
        (let ((x1 (car set1)) 
              (x2 (cdr set1))
              (y1 (car set2))
              (y2 (cdr set2)))
          (cond ((and (= x1 y1) (null? x2) (null? y2)) 
                 set1)
                ((and (< x1 y1) (null? x2) (null? y2)) 
                 (list x1 y1)) 
                ((and (> x1 y1) (null? x2) (null? y2)) 
                 (list y1 x1)) 
                ((> x1 y1) 
                 (cons y1 (union-set set1 (cdr set2))))
                ((< x1 y1) 
                 (cons x1 (union-set (cdr set1) set2)))
                ((= x1 y1) 
                 (union-set (cdr set1) set2)))))))

(define (intersection-set set1 set2) 
  (list->tree (intersection-list-set (tree->list2 set1) 
                                     (tree->list2 set2))))

(define (union-set set1 set2) 
  (list->tree (union-list-set (tree->list2 set1) 
                              (tree->list2 set2))))

; Haven't tested, but should work I hope! 

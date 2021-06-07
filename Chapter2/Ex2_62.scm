
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

(define (intersection-set set1 set2) 
  (if (or (null? set1) (null? set2)) 
      '() 
      (let ((x1 (car set1)) (x2 (car set2))) 
        (cond ((= x1 x2) (cons x1 
                               (intersection-set (cdr set1) 
                                                 (cdr set2)))) 
              ((< x1 x2) (intersection-set (cdr set1) set2)) 
              ((< x2 x1) (intersection-set set1 (cdr set2)))))))

; x1 is the first element of set1
; y1 is the first element of set1
; x2 is the rest of set1
; y2 is the rest of set1

(define (union-set set1 set2) 
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



; Tests 

(union-set null null)

(union-set null (list 1))

(union-set (list 1) null)

(union-set (list 1) (list 2))

(union-set (list 2) (list 1))

(union-set (list 3) (list 1 2))

(union-set (list 1 2) (list 3))

(union-set (list 1 3) (list 2))

(union-set (list 1 3) (list 2 4))

(union-set (list 1 3) (list 3 4))





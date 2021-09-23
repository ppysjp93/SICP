(define (lookup given-key set-of-records) 
  (cond ((null? set-of-records) false) 
        ((equal? given-key (key (car set-of-records))) 
         (car set-of-records)) 
        (else (lookup given-key (cdr set-of-records)))))

(define (lookup x set-of-records) 
  (cond ((null? set-of-records) false) 
        ((= x (entry set-of-records)) (entry set-of-records)) 
        ((< x (entry set-of-records)) 
         (lookup x (left-branch set-of-records)))
        ((> x (entry set set-of-records)) 
         (lookup x (right-branch set-of-records)))))

; Taken from Ex2_63.scm

(define tree1 (make-tree 7 sub-tree1 sub-tree2))

(lookup 3 tree1)


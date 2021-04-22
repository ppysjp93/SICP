(define x (list 1 2 3))

(define (subsets s) 
  (if (null? s) 
      (list nil) 
      (let ((rest (subsets (cdr s)))) 
        (append rest 
                (map (lambda (subset) (list (car s) subset)) 
                          rest)))))

(subsets x)

(list null (list 3) (list 2) (list 2 3) (list 1) (list 1 3) (list 1 2) (list 1 2 3))

; Answer 

(define (subsets s) 
  (if (null? s) 
      (list nil) 
      (let ((rest (subsets (cdr s)))) 
        (append rest 
                (map (lambda (subset) (cons (car s) subset)) 
                          rest)))))

(subsets x)

; I just changed the way the list was built in the lambda to cons because 
; cons are whats used to construct lambdas! 

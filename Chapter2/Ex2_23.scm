(define (foreach proc items) 
  (proc (car items)) 
  (if (null? (cdr items)) 
      (newline)  
      (foreach proc (cdr items))))

(foreach (lambda (x) (newline) (display x)) (list 57 321 88))

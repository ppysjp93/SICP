(define (scale-list items factor) 
  (if (null? items) 
      nil 
      (cons (* (car items) factor) (scale-list (cdr items) factor))))

(scale-list (list 1 2 3 4 5) 10)

(define (map proc items) 
  (if (null? items) 
      nil 
      (cons (proc (car items)) 
            (map proc (cdr items)))))

(map abs (list -10 2.5 -11.6 17))

(map (lambda (x) (* x x)) 
     (list 1 2 3 4 5))

; Now we can redefine scale-list using map 

(define (scale-list items factor) 
  (map (lambda (x) (* x factor))  items))

(scale-list (list 1 2 3 4 5) 10)



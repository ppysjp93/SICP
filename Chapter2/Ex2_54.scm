(define (equal? list1 list2) 
  (if (null? list1) 
      true 
      (and (eq? (car list1) 
                (car list2)) 
           (equal? (cdr list1) (cdr list2)))))



(equal? '(this is a list) '(this (is a ) list))
(equal? '(this is a list) '(this is a list))


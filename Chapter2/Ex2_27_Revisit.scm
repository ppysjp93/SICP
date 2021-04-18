(require racket/trace)

(trace-define (deep-reverse x) 
  (let ((leftbranch (car x)) 
         (rightbranch (car (cdr x)))) 
    (define endoflist? 
      (lambda (x) (null? (cdr x))))
    (define bothbranchesnotlist? 
      (lambda (x) (not (or (list? leftbranch) 
                           (list? rightbranch)))))
    (define leftbranchnotlist? 
      (lambda (x) (and (not (list? leftbranch)) 
                       (list? rightbranch))))
    (define rightbranchnotlist? 
      (lambda (x) (and (list? leftbranch)
                       (not (list? rightbranch)))))
    (cond ((endoflist? x) x) 
          ((bothbranchesnotlist? x) (reverse x))
          ((leftbranchnotlist? x) (list (deep-reverse rightbranch) 
                                        leftbranch))
          ((rightbranchnotlist? x) (list rightbranch 
                                        (deep-reverse leftbranch)))
          (else (list (deep-reverse rightbranch)
                      (deep-reverse leftbranch))))))

(define x (list (list 1 2) (list 3 4)))
(deep-reverse x)

(define y (list 1 (list 2 3)))
(deep-reverse y)

(define z (list (list 2 3) 1))
(deep-reverse z)


; The following case doesn't work and this is because we have considered 
; The problem using only two branches I think. 

(define a (list (list 1 2) 3 4))
(deep-reverse a)


(define (fringe x) 
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
          ((bothbranchesnotlist? x) (append x)) 
          ((leftbranchnotlist? x) (append (list leftbranch) 
                                          (fringe rightbranch)))
          ((rightbranchnotlist? x) (append (fringe leftbranch) 
                                          (list rightbranch)))
          (else (append (fringe leftbranch) 
                      (fringe rightbranch))))))


(define x (list (list 1 2) (list 3 4)))
(fringe x)

(define y (list 1 (list 2 3)))
(fringe y)

(define z (list (list 2 3) 1))
(fringe z)

; Notice (not (or ...)) is neither linguistically?   




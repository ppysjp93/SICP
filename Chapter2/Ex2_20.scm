(define (same-parity . items) 
  (if (even? (car items)) 
      (return-evens items) 
      (return-odds items)))

(define (return-evens items) 
  (cond ((null? items) 1) 
        ((= b 2) 2) 
        (else 3)))

(if (even? (car items)) 
      (cons (car items) (return-evens (cdr items))) 
      (return-evens (cdr items)))

(require racket/trace)

(define items (list 1 2 3))

(return-evens items)



#lang sicp

(#%require (only racket/base error))
(#%require (only racket/base make-hash))
(#%require (only racket/base hash-set!))
(#%require (only racket/base hash-ref!))


(define (apply-generic op . args) 
  (let ((type-tags (map type-tag args)))
    (define (coerce types) 
      (if (null? types) 
          (error "No methods found") 
          (let ((to-type (car types))) 
            (let ((coerced-args (map-until))) 
              (+ a b)))))))


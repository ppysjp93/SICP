#lang sicp
(#%require (only racket/base error))
(#%require (only racket/base make-hash))
(#%require (only racket/base hash-set!))
(#%require (only racket/base hash-ref))

(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) '()))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
     (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
(and (number? exp) (= exp num)))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (install-sum-pkg)
  (define (make a1 a2)
     (cond ((=number? a1 0) a2)
           ((=number? a2 0) a1)
           ((and (number? a1) (number? a2)) (+ a1 a2))
           (else (list '+ a1 a2))))  
  (define (derivative operands var)
    ((get 'make '+) 
     (deriv (car operands) var)
     (deriv (cadr operands) var)))
  (put 'make '+ make)
  (put 'deriv '+ derivative))  

(define (install-product-pkg) 
  (define (make m1 m2)
    (cond ((or ( =number? m1 0) (=number? m2 0)) 0) 
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2)))) 
  (define (derivative operands var) 
    ((get 'make '+) 
     ((get 'make '*) (car operands) 
                     (deriv (cadr operands) var)) 
     ((get 'make '*) (deriv (car operands) var) 
                     (cadr operands)))) 
  (put 'make '* make) 
  (put 'deriv '* derivative))

(define (install-exponentiation-pkg) 
  (define (make base exponent) 
    (cond ((=number? exponent 0) 1) 
          ((=number? exponent 1) base) 
          ((and (number? base) (number? exponent))
           (expt base exponent))
          (else (list '** base exponent)))) 
  (define (derivative operands var) 
    ((get 'make '*) ((get 'make '*) 
                     (cadr operands) 
                     ((get 'make '**)
                      (car operands) 
                      ((get 'make '+) 
                       (cadr operands) 
                       -1))) 
                    (deriv (car operands) var))) 
  (put 'make '** make) 
  (put 'deriv '** derivative))

; If we could reference the code in the top half then the below 
; code could be considered a class and the installs are what is being 
; used. 

(install-sum-pkg)
(install-product-pkg)
(install-exponentiation-pkg)

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) 
                (operands exp) 
                var))))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(** x 2) 'x)
(deriv '(+ (** x 3) (* 2 (** x 2)))  'x)


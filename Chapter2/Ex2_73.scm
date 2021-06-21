#lang Racket

; We need a definition for put and get, we can use the definition that is 
; give on page 271. 
; We also have to run the solution in drracket under the SICP mode
; otherwise I don't think it will work

 (define *the-table* (make-hash));make THE table 
 (define (put key1 key2 value) (hash-set! *the-table* (list key1 key2) value));put 
 (define (get key1 key2) (hash-ref *the-table* (list key1 key2) #f));get 

; Method From Symbolic differentiation

(define (variable? x) 
  (symbol? x))

(define (same-variable? v1 v2) 
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (sum? x) 
  (and (pair? x) (eq? (car x) '+)))


;(define (deriv exp var) 
;  (cond ((number? exp) 0) 
;        ((variable? exp) (if (same-variable? exp var) 1 0)) 
;        ((sum? exp) 
;         (make-sum (deriv (addend exp) var) 
;                   (deriv (augend exp) var))) 
;        ((product? exp) 
;         (make-sum (make-product (multiplier exp) 
;                                 (deriv (multiplicand exp) var)) 
;                   (make-product (deriv (multiplier exp) var) 
;                                 (multiplicand exp))))
;        ;more rules can be added here
;        (else (error "unknown expression type -- DERIV" exp))))

; First Test 

;(display (deriv '(+ x 3) 'x))

(define (deriv exp var) 
  (cond ((number? exp) 0) 
        ((variable? exp) (if (same-variable? exp var) 1 0)) 
        (else ((get 'deriv (operator exp)) (operands exp) 
                                           var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

; We now need to create a sum-deriv package so that we can 
; differentiate a sum. 

(define (attach-tag type-tag contents) 
  (cons type-tag contents))

(define (type-tag datum) 
  (if (pair? datum) 
      (car datum) 
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum) 
  (if (pair? datum) 
      (cdr datum) 
      (error "Bad tagged datm -- CONTENTS" datum)))

(define (install-sum-deriv) 
  ;; internal procedures
  (define (make-sum a1 a2) (list a1 a2))
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  (define (sum-deriv exp var) 
    (make-sum (deriv (addend exp) var) 
              (deriv (augend exp) var)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag '+ x))
  (put 'deriv '+ 
       (lambda (exp var) (tag (sum-deriv exp var))))
  'done)

; The below should return a procdedure but I'm getting false

;(display (deriv '(+ x 3) 'x))


(get 'deriv '+)


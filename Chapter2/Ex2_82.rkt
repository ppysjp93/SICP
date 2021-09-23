#lang sicp

(#%require (only racket/base error))
(#%require (only racket/base make-hash))
(#%require (only racket/base hash-set!))
(#%require (only racket/base hash-ref))

; Coercion Table

(define *coercion-table* (make-hash))

(define (put-coercion type1 type2 proc)
  (hash-set! *coercion-table* (list type1 type2) proc))

(define (get-coercion type1 type2)
  (hash-ref *coercion-table* (list type1 type2) '()))

; Procedure Table

(define *proc-table* (make-hash))

(define (put op type proc)
  (hash-set! *proc-table* (list op type) proc))

(define (get op type)
  (hash-ref *proc-table* (list op type) '()))

; Tagging Procedures

(define (attach-tag type-tag contents) 
 (cons type-tag contents))

(define (type-tag datum) 
  (cond ((number? datum) 'scheme-number) 
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum) 
  (cond ((number? datum) datum) 
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))

(define (map-until until-val proc args) 
  (if (null? args) 
      '() 
      (let ((arg (car args))) 
        (let ((marg (proc arg))) 
          (if (equal? marg until-val) 
              '() 
              (cons marg (map-until until-val proc (cdr args))))))))

(define (apply-generic op . args) 
  (let ((type-tags (map type-tag args))) 
    (define (coerce types) 
      (if (null? types) 
          (error "No methods found") 
          (let ((to-type (car types))) 
            (let ((coerced-args 
                    (map-until 
                      '() 
                      (lambda (arg) 
                        (let ((from-type (type-tag arg))) 
                          (if (equal? to-type from-type) 
                              arg 
                              (let ((coerce-proc (get-coercion from-type to-type))) 
                                (if (null? coerce-proc) '() (coerce-proc arg)))))) 
                      args))) ; args might need shifting one set of parens
              (if (equal? (size coerced-args) (size args)) 
                  (apply apply-generic op coerced-args) 
                  (coerce (cdr types)))))))
  (let ((proc (get op type-tags))) 
    (if (not (null? proc)) 
        (apply proc (map contents args)) 
        (if (accumulate 
              (lambda (a b) (and a b)) 
              #t 
              (map 
                (lambda (x) (equal? (car type-tags) x)) 
                (cdr type-tags))) 
            (error "No method found for *same* types" (list op type-tags)) 
            (coerce type-tags))))))

(define (square x) (* x x))

(define (size list) 
  (if (null? list) 0 (+ 1 (size (cdr list)))))

(define (filter predicate sequence) 
  (cond ((null? sequence) nil) 
        ((predicate (car sequence)) 
         (cons (car sequence) 
               (filter predicate (cdr sequence)))) 
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence) 
  (if (null? sequence) 
      initial 
      (op (car sequence) 
          (accumulate op initial (cdr sequence)))))

(define (install-scheme-number-package) 
  (define (tag x) 
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number) 
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number) 
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number) 
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number) 
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number) =)
  (put 'exp '(scheme-number scheme-number) 
       (lambda (x y) (tag (expt x y)))))














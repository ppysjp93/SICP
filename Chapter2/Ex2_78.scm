#lang sicp
(#%require (only racket/base error))
(#%require (only racket/base make-hash))
(#%require (only racket/base hash-set!))
(#%require (only racket/base hash-ref))
(#%require racket/trace)

; I've created 2 tables that store the general procedures for the 
; arithmetic operations (Rational, Complex Ordinary) and the general 
; procedures for the complex numbers (rectangular/polar)

(define *proc-table* (make-hash))

(define (put op type proc)
  (hash-set! *proc-table* (list op type) proc))

(define (get op type)
  (hash-ref *proc-table* (list op type) '()))

(define (apply-generic op . args) 
  (let ((type-tags (map type-tag args))) 
    (let ((proc (get op type-tags))) 
      (if proc 
          (apply proc (map contents args)) 
          (error 
            "No method for these types -- APPLY-GENERERIC" 
            (list op type-tags))))))


(define (attach-tag type-tag contents) 
 (if (number? contents) 
     contents 
     (cons type-tag contents)))


(define (type-tag datum) 
  (cond ((number? datum) 'scheme-number) 
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))


(define (contents datum) 
  (cond ((number? datum) datum) 
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))

; General Procedures for Arithmetic

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

; General Procedures for Complex Numbers

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

; First Arithmetic Package

(define (install-scheme-number-package) 
  (define (tag x) 
    (attach-tag 'scheme-number x)) 
  (put 'add '(scheme-number scheme-number) 
       (lambda (x y) (+ x y)))
  (put 'sub '(scheme-number scheme-number) 
       (lambda (x y) (- x y)))
  (put 'mul '(scheme-number scheme-number) 
       (lambda (x y) (* x y))) 
  (put 'div '(scheme-number scheme-number) 
       (lambda (x y) (/ x y)))
  (put 'make 'scheme-number 
       (lambda (x) (tag x))))

(install-scheme-number-package)

(add 3 4)



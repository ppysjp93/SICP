#lang sicp
(#%require (only racket/base error))
(#%require (only racket/base make-hash))
(#%require (only racket/base hash-set!))
(#%require (only racket/base hash-ref))
(#%require racket/trace)

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
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

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
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number) 
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number) 
       (lambda (x) (= x 0))))

; Second Arithmetic Package

(define (install-rational-package) 
  ;; internal procedures
  (define (numer x) 
    (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d) 
    (let ((g (gcd n d))) 
      (cons (/ n g) (/ d g))))
  (define (add-rat x y) 
    (make-rat (+ (* (numer x) (denom y)) 
                 (* (numer y) (denom x))) 
              (* (denom x) (denom y)))) 
  (define (sub-rat x y) 
      (make-rat (- (* (numer x) (denom y)) 
                   (* (numer y) (denom x))) 
                (* (denom x) (denom y))))
  (define (mul-rat x y) 
        (make-rat (* (numer x) (numer y)) 
                  (* (denom x) (denom y))))
  (define (div-rat x y) 
          (make-rat (* (numer x) (denom y)) 
                    (* (denom x) (numer y))))
  (define (print-rat x) 
  (newline)
  (display (numer x)) 
  (display "/") 
  (display (denom x)) 
  (newline))

  (define (equal-rat? x y) 
    (and (= (numer x) (numer y)) 
         (= (denom x) (denom y))))

  ;;interface to rest of the system
  (define (tag x) (attach-tag 'rational x)) 
  (put 'add '(rational rational) 
       (lambda (x y) (tag (add-rat x y)))) 
  (put 'sub '(rational rational) 
         (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational) 
           (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational) 
             (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational (lambda (n d) 
                         (tag (make-rat n d))))
  (put'print 'rational 
       (lambda (x) (print-rat (contents x))))
  (put 'equ? '(rational rational) 
       (lambda (x y) (equal-rat? x y)))
  (put '=zero? '(rational) 
       (lambda (x) (= (numer x) 0))))


; First Complex Package

(define (install-rectangular-package) 
  ;; internal procedures
  (define (square x) (* x x)) 
  (define (real-part z) (car z)) 
  (define (imag-part z) (cdr z)) 
  (define (make-from-real-imag x y) (cons x y)) 
  (define (magnitude z) 
    (sqrt (+ (square (real-part z)) 
             (square (imag-part z))))) 
  (define (angle z) 
    (atan (imag-part z) (real-part z))) 
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part) 
  (put 'imag-part '(rectangular) imag-part) 
  (put 'magnitude '(rectangular) magnitude) 
  (put 'angle '(rectangular) angle) 
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y)))) 
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a)))))

; Second Complex Package 

(define (install-polar-package) 
  ;; internal procedures
  (define (square x) (* x x))
  (define (magnitude z) (car z)) 
  (define (angle z) (cdr z)) 
  (define (make-from-mag-ang r a) 
    (cons r a)) 
  (define (real-part z) (* (magnitude z) (cos (angle z)))) 
  (define (imag-part z) (* (magnitude z) (sin (angle z)))) 
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y))) (atan y x))) 
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part) 
  (put 'imag-part '(polar) imag-part) 
  (put 'magnitude '(polar) magnitude) 
  (put 'angle '(polar) angle) 
  (put 'make-from-real-imag 'polar 
       (lambda (x y) (tag (make-from-real-imag x y)))) 
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a)))))

(install-rectangular-package)
(install-polar-package)

; Third Arithmetic Package

(define (install-complex-package) 
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y) 
    ((get 'make-from-real-imag 'rectangular) x y)) 
  (define (make-from-mag-ang r a) 
    ((get 'make-from-mag-ang 'polar) r a)) 
  (define (real-part x) 
      ((get 'real-part '(rectangular)) 
       (contents x)))
  (define (imag-part x) 
        ((get 'imag-part '(rectangular)) 
         (contents x)))

  ;; internal procedures
  (define (make-rectangular-or-polar z1 z2) 
    (if (eq? (and (type-tag z1) (type-tag z2)) 'rectangular) 
        make-from-real-imag 
        make-from-mag-ang)) 

  (define (add z1 z2) 
    ((make-rectangular-or-polar z1 z2) 
     (+ (real-part z1) (real-part z2))
     (+ (imag-part z1) (imag-part z2))))
  (define (sub z1 z2) 
      ((make-rectangular-or-polar z1 z2) 
       (- (real-part z1) (real-part z2))
       (- (imag-part z1) (imag-part z2))))
  (define (mul z1 z2) 
        ((make-rectangular-or-polar z1 z2) 
         (* (magnitude z1) (magnitude z2))
         (+ (angle z1) (angle z2))))
  (define (div z1 z2) 
          ((make-rectangular-or-polar z1 z2) 
           (/ (magnitude z1) (magnitude z2))
           (- (angle z1) (angle z2))))
  (define (equal-complex? x y) 
    (and (= (real-part x) (real-part y)) 
         (= (imag-part x) (imag-part y))))  

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z)) 
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'add '(complex complex) 
       (lambda (z1 z2) (tag (add z1 z2)))) 
  (put 'sub '(complex complex) 
         (lambda (z1 z2) (tag (sub z1 z2))))
  (put 'mul '(complex complex) 
           (lambda (z1 z2) (tag (mul z1 z2))))
  (put 'div '(complex complex) 
             (lambda (z1 z2) (tag (div z1 z2))))
  (put 'make-from-real-imag 'complex 
               (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex 
                 (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ? '(complex complex) 
       (lambda (x y) (equal-complex? x y)))
  (put '=zero? '(complex) 
       (lambda (x) (and (= (real-part x) 0) 
                        (= (imag-part x) 0)))))

(install-scheme-number-package)
(install-rational-package)
(install-complex-package)

(display "Scheme Number Arithmetic")
(newline)

(define (make-scheme-number n) 
  ((get 'make 'scheme-number) n))

(define two (make-scheme-number 2))

(equ? two two)

(=zero? two)

 
(display "Rational Number Arithmetic")
(newline)

(define (make-rat n d) 
  ((get 'make 'rational) n d))

(define (display-rat x) 
  ((get 'print 'rational) x))

(define two-thirds (make-rat 2 3))

(equ? two-thirds two-thirds)

(=zero? two-thirds)
 
(display "Complex-Number Arithmetic")
(newline)

(define (make-rectangular x y) 
  ((get 'make-from-real-imag 'complex) x y))

(define z1 (make-rectangular 3 4))

(equ? z1 z1) 

(=zero? z1)


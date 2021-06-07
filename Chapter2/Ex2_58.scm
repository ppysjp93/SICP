; Part 1: Is just a rearrangement of the car, cadr and caddr now that the 
; symbol has moved to the middle, fairly straight forward.

(define (make-sum a1 a2) 
  (cond ((=number? a1 0) a2) 
        ((=number? a2 0) a1) 
        ((and (number? a1) (number? a2)) (+ a1 a2)) 
        (else (list a1 '+ a2))))

(define (make-product m1 m2) 
  (cond ((or (=number? m1 0) (=number? m2 0)) 0) 
        ((=number? m1 1) m2) 
        ((=number? m2 1) m1) 
        ((and (number? m1) (number? m2)) (* m1 m2)) 
        (else (list m1 '* m2))))

(define (make-product m1 m2) (list m1 '* m2))

(define (sum? x) 
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (product? x) 
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

(display (deriv '(x + 3) 'x)) 

(display (deriv '(x * y) 'x)) 

(display (deriv '((x * y) * (x + 3)) 'x)) 

(display (deriv '(x + (3 * (x + (y + 2)))) 'x)) 


; Part 2: 

; In part 1 we were able to use caddr to get the augend or multiplicand
; This is not possible for the case where we allow standard algebraic 
; notation.

; We want to deal with the entire sub-expression now, which means that we 
; use the cddr instead to define the augend and multiplicand. 

(cddr '(x + 3))

(define (augend s) (cddr s))

(define (multiplicand p) (cddr p))

(display (deriv '(x + 3 * (x + y + 2)) 'x)) 

; Unfortunately this doesn't work because we have no way of dealing with 
; the null term, but we still want to deal with the cddr. 
; The way round this is to simplify the expression. If the sub-expression 
; is made up of two or more elements we want the whole sub-expression
; if it is made up of one element then we only want the first element. 
; simplify almost acts like a list-switch, depending on whether we are dealing
; with the final symbol in a list. 

(define (simplify exp) 
  (if (null? (cdr exp)) (car exp) exp))

(define (augend s) (simplify (cddr s)))

(define (multiplicand p) (simplify (cddr p)))

(display (deriv '(x + 3 * (x + y + 2)) 'x)) 





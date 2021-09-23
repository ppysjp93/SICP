; this is a procedural representation of zero. It takes a procedure as argument
; and then applies that procedure zero times. 

(define zero (lambda (f) (lambda (x) x)))

; let's now test it out with inc

(define (inc x) 
  (+ x 1))

((zero inc) 3)

; as you can see it applies the function inc zero times to 3.

(define one (lambda (f) (lambda (x) (f x))))

((one inc) 3)

(define two (lambda (f) (lambda (x) (f (f x)))))

((two inc) 3)

; cool makes a lot of sense. 

; composing is a procedure we have defined in the past which takes two 
; procedural arguments the definition of plus we make will have to take two 
; arguments

(define (compose f g) 
  (lambda (x) (f (g x))))

; this isn't quite quite close to what we are after. 
; we are told that add-1 is defined as below

(define (add-1 n) 
 (lambda (f) (lambda (x) (f ((n f) x)))))

; So this should add-1

(((add-1 one) inc) 2)

; So we can use the idea  two create plus

(define (church-plus a b) 
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

(((church-plus one two) inc) 3)

; This is nown as church numerals. Like roman numerals numbers are represented
; using IVX and so on. In church numerals numbers are represented using lambdas




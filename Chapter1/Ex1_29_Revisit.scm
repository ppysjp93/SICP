(require racket/math)

; So first thing first let's get our higher order function set up

(define (sum term a next b) 
  (if (> a b) 
      0 
      (+ (term a)
         (sum term (next a) next b))))

; The only constant we have to define is h

(let ((h (/ (- b a) n))) 
  (+ a b))

; We now define our y terms in terms of k 

(define (y k) 
  (f (+ a (* k h))))

; We then define our coefficients for our y terms in terms of T
; T is also a function of k

(define (T k) 
  (cond ((or (= k 0) (= k n)) 1) 
        ((even? k) 2) 
        (else 4)))

; We our now in a position to define our procedural arguments
; Because these are relatively simple operations I've replaced
; them with lambdas in the main function definition.
(define (next k) (+ k 1))
(define (term k) (* (y k) (T k)))

; xWe now put everything we know together to make our simpson integral function

(define (simpson f a b n) 
  (let ((h (/ (- b a) n))) 
  (define (y k) 
    (f (+ a (* k h))))
  (define (T k) 
    (cond ((or (= k 0) (= k n)) 1) 
          ((even? k) 2) 
          (else 4)))
  (* (/ h 3) 
     (sum (lambda (k) (* (T k) (y k))) 
          0 
          (lambda (k) (+ k 1))
          n))))

; Finally we test it

(define (cube x) (* x x x))

(require racket/trace)
(trace sum)
(simpson cube 0.0 1.0 10)




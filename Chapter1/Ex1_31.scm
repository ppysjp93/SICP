(define (product-cubes a b) 
  (if (> a b) 
      1
      (* (cube a) 
         (product-cubes (+ a 1) b))))

(product-cubes 1 3)

; Ok so now we will abstract the idea of product.

(define (product term a next b) 
  (if (> a b)
      1
      (* (term a) 
         (product term (next a) next b))))

; So now let's use our higher order function of product to make product-cubes
; again

(define (product-cubes a b) 
  (define (term x) 
    (* x x x))
  (define (next x) 
    (+ x 1))
  (product term a next b))

(product-cubes 1 3)

; Ok so we now have our higher order function that is much easier to set up
; than trying to think through the logic for the next problem.
; We can now define factorial using product

(define (factorial n) 
  (define (term x) x)
  (define (next x) 
    (+ x 1))
  (product term 1 next n))

(factorial 5)

; Finally we can do our pi-approx

(define (pi-approx b) 
  (define (square x) 
    (* x x))
  (define (term x) 
    (/ (* x (+ x 2)) 
       (square (+ x 1))))
  (define (next x) 
    (+ x 2))
 (* 4 (product term 2.0 next b)))

(pi-approx 10.0)

(pi-approx 1000.0)

; Obviously we could go further here and define pi-approx so that it takes no
; formal arguments but I like that we can vary the accuracy of the approximation

; iterative form of product. Let's see if it works

(define (product term a next b) 
  (define (iter a result) 
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
 (iter a 1))

(define (product-cubes a b) 
  (define (term x) 
    (* x x x))
  (define (next x) 
    (+ x 1))
  (product term a next b))

(product-cubes 1 3)

; So to finish up we just have to send our new definiton of iterative product
; and run it for our new defitions for factorial and pi-approx.

(define (factorial n) 
  (define (term x) x)
  (define (next x) 
    (+ x 1))
  (product term 1 next n))

(factorial 5)

(define (pi-approx b) 
  (define (square x) 
    (* x x))
  (define (term x) 
    (/ (* x (+ x 2)) 
       (square (+ x 1))))
  (define (next x) 
    (+ x 2))
 (* 4 (product term 2.0 next b)))

(pi-approx 10.0)

(pi-approx 1000.0)



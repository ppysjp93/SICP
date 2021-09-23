(define (sum term a next b) 
  (if (> a b)
      0
      (* (term a) 
         (sum term (next a) next b))))

(define (product term a next b) 
  (if (> a b)
      1
      (* (term a) 
         (product term (next a) next b))))

; So we are asked to make a higher order function of these to functions. 

(define (accumulate combiner null-value term a next b) 
  (if (> a b) 
      null-value
      (combiner (term a) 
                (accumulate combiner null-value term (next a) next b))))

; when we pass in comibiner it will be a function that takes two arguments
; So this is the second level higher order function that we can use to find the 
; sum of cubes for example

(define (sum-cubes a b) 
  (define (combiner current previous) 
    (+ current previous))
  (define (term x) 
    (* x x x))
  (define (next x) 
    (+ x 1))
  (accumulate combiner 0 term a next b))

(sum-cubes 1 3)

(define (product-cubes a b) 
  (define (combiner current previous) 
    (* current previous))
  (define (term x) 
    (* x x x))
  (define (next x) 
    (+ x 1))
  (accumulate combiner 1 term a next b))

(product-cubes 1 3)

; This is just so nice, it means that I can just slot my functions in to my 
; higher order function and it will just compute nicely!


(define (sum-integers a b) 
  (if (> a b)
    0 
    (+ a (sum-integers (+ a 1) b))))

(sum-integers 1 10)

(define (sum-integers a b) 
  (define (combiner current previous) 
    (+ current previous))
  (define (term x) 
    x)
  (define (next x) 
    (+ x 1))
  (accumulate combiner 0 term a next b))

; Let's now make an iterative form of accumulate

; First lets consider the higher order sum function

(define (sum term a next b) 
  (define (iter a result) 
    (if (> a b) 
        result
        (iter (next a) (+ result (term a))))) 
  (iter a 0))
 
(define (sum-integers a b) 
  (define (term x) x)
  (define (next x) (+ x 1))
  (sum term a next b))

(sum-integers 1 10)

; Now we go ahead and make the 2nd higher order function accumlate

(define (accumulate combiner null-value term a next b) 
  (define (iter a result) 
    (if (> a b) 
        result
        (iter (next a) (combiner (term a) result)))) 
  (iter a null-value))

; And now we have our iterative accumulate 2nd higher order function

(define (sum-cubes a b) 
  (define (combiner current previous) 
    (+ current previous))
  (define (term x) 
    (* x x x))
  (define (next x) 
    (+ x 1))
  (accumulate combiner 0 term a next b))

(sum-cubes 1 3)

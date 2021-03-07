
; the lambda special form gives us a new way of defining functions without using
; the define special form
; this is an unevaluated version of the procedure. 
; to evaluate the lambda you have to pass it 

(lambda (x) (+ x 4))

; Here we are using the lambda as the operator of a combination

((lambda (x) (+ x 4)) 2)

; Let's define our higher order sum function

(define (sum term a next b) 
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (sum-cubes a b) 
  (define (cube x) 
    (* x x x))
  (define (next x) 
    (+ x 1))
  (sum cube a next b))

(sum-cubes 1 4)

; 64 + 27 + 8 + 1 = 65 + 35 = 100 

; Ok so now we can redefine the sum-cubes by just using straight up lambdas 
; as arguments for our higher order function. There must be some benefit to this
; that I will find later on the more I use these 'lambdas'

(define (sum-cubes a b) 
  (sum (lambda (x) (* x x x)) 
       a 
       (lambda (x) (+ x 1))
       b))

(sum-cubes 1 4)

; It definitely feels a little more concise way of writing things. 

(define (pi-sum a b) 
  (sum (lambda (x) (/ 1.0 (* x (+ x 2)))) 
       a 
       (lambda (x) (+ x 4))
       b))

(pi-sum 1 10)

; We can also define the integral from before using the lambda special form

(define (integral f a b dx) 
  (* (sum f 
       (+ a (/ dx 2.0)) 
       (lambda (x) (+ x dx))
       b) dx))

(define (cube-integral a b dx) 
  (integral (lambda (x) (* x x x)
            a
            b
            dx)))

; When writing procedures we sometimes want ot be able to bind local variables.
; We do this by using auxillary procedures. In this case a and b are the 
; local variables that are being bound. The problem wit this method is that
; the local variables are bound at the end when the formal parameters of the 
; helper function are defined. 

(define (f x y) 
  (define (square x) (* x x))
  (define (f-helper a b) 
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
  (- 1 y)))

; Instead we could use a lambda expression to specify an anonymous procedure for
; binding our local varibes. This reduces the amount of syntax somewhat to get
; the desired function.

(define (f x y) 
  (define (square x) (* x x x))
  ((lambda (a b) 
    (+ (* x (square a))
       (* y b) 
       (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

; We are now introduced to the 'let' special form which allows us to bind 
; local variables in a more natural way as we are able to do it at the 
; start of a function definition

(define (f x y) 
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))

    ))



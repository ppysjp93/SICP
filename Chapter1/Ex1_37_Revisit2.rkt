; I think it's important to ask your self when creating a function
; Is the argmument a primitive argument? 
; Is the argument a procedural argument

; I'm now going to attempt to create the higher order version of the continuous
; fraction. n, d and next are all procedural arguments. i is the only primitive.

(define (cont-frac-rec n d next i k) 
  (if (= i k) 
      (/ (n i) (d i)) 
      (/ (n i) (+ (d i) (cont-frac-rec n d next (next i) k)))))

(define (cont-frac n d k) 
  (cont-frac-rec n d (lambda (i) (+ i 1))  1 k))

(cont-frac (lambda (i) 1.0) 
           (lambda (i) 1.0) 
           10)

; I want to make cont-frac-iter because it uses less space. I'm going to 
; refactor the rec further by adding in a combine procedural argument


(define (cont-frac-rec n d next combine i k) 
  (if (= i k) 
         (/ (n i) (d i)) 
         (combine (n i) 
                   (d i) 
                   (cont-frac-rec n d next combine (next i) k))))

(define (cont-frac n d k) 
  (cont-frac-rec n 
                 d 
                 (lambda (i) (+ i 1))  
                 (lambda (n d f) (/ n (+ d f))) 
                 1 
                 k))

(cont-frac (lambda (i) 1.0) 
           (lambda (i) 1.0) 
           10)

; Now I want to make an iterative form of my cont-frac-rec

(define (sum term a next b) 
  (define (iter a result) 
    (if (> a b)
        result
        (iter (next a) 
              (+ result (term a)))))
  (iter a 0))

; Here we have used our template from Ex1_31.scm to get our higher order function.
; As far as I can tell the easiest way to iterate this is to work from the 
; bottom of the fraction and to then work your way back up.

(define (cont-frac-iter n d next combine i k) 
  (define (iter i result) 
    (if (= i 1) 
        result 
        (iter (next i) (combine (n i) (d i) result))))
  (iter k (/ (n k) (d k))))

(define (cont-frac n d k) 
  (cont-frac-iter n 
                 d 
                 (lambda (i) (- i 1))  
                 (lambda (n d f) (/ n (+ d f))) 
                 1 
                 k))

(cont-frac (lambda (i) 1.0) 
           (lambda (i) 1.0) 
           10)

; This still isn't quite right. See Ex1_38_Revisit.scm

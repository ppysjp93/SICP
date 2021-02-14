; Linear Recursive design

(define (exponent b n) 
  (if (= n 0)
    1
    (* b (exponent b (- n 1)))))

;Linear Iterative design

(define (expt1 b n) 
  (expt-iter b n 1))

(define (expt-iter b counter product) 
  (if (= counter 0) 
      product (expt-iter b 
                         (- counter 1) 
                         (* b product))))

; Recursive Logarthmic Design

(define (even?? n) 
  (= (remainder n 2) 0))

(define (square x) 
  (* x x))

(define (fast-expt b n) 
  (cond ((= n 0) 1) 
        ((even?? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

;Try Changing the value of n to something very large and see how all 3 perform

;(exponent 3 1000000)

;(expt1 3 1000000)

(fast-expt 3 1000000)

; See Ex1_16.scm for Iterative Logarithmic Design


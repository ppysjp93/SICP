; Turn this solution into a fixed point some time. 
; Notes about this puzzle are in the back of SICP Notebook 2
; Date: 03/05/2021

(require racket/math)

(define (sum term a next b) 
  (if (> a b) 
      0 
      (+ (term a) 
         (sum term (next a) next b))))

(define (square x) (* x x))

(define (sum-of-squares x y) 
(+ (square x) (square y)))

(define (root-sum-of-squares x y) 
  (sqrt (sum-of-squares x y)))

(define (sum-odd-n n) 
  (sum (lambda (n) (- (* 2 n) 1))  
       1 
       (lambda (n) (+ n 1)) 
       n))

(define (calc-y1 n) 
  (lambda (H) 
    (/ H (sum-odd-n n))))


(define (delta-y n) 
  (* (- (* 2 n) 1) y1))

(define (sum-roots n) 
  (sum (lambda (n) 
         (root-sum-of-squares delta-x 
                              (delta-y n))) 
       1 
       (lambda (n) (+ n 1))  
       n))


(define L 80.0)
(define H 30.0)

(define n 1)
(define y1 ((calc-y1 n) H))
(define delta-x (* 10 (sqrt 7)))
(sum-roots n)
(- (/ L 2) 
   (sum-roots n))
(define answer (* 2 n delta-x))
answer


(define n 2)
(define y1 ((calc-y1 n) H))
(define delta-x 12)
(sum-roots n)
(- (/ L 2) 
   (sum-roots n))
(define answer (* 2 n delta-x))
answer

(define n 3)
(define y1 ((calc-y1 n) H))
(define delta-x 8)
(sum-roots n)
(- (/ L 2) 
   (sum-roots n))
(define answer (* 2 n delta-x))
answer


(define n 4)
(define y1 ((calc-y1 n) H))
(define delta-x 5.92)
(- (/ L 2) 
   (sum-roots n))
(define answer (* 2 n delta-x))
answer







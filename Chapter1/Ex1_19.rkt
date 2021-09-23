(define (fib n) 
  (fib-iter 1 1 0 1 n))


(define (even? a) 
  (= (remainder a 2) 0))

(define (square a) 
  (* a a))

(define (sum-square a b) 
  (+ (square a) (square b)))

(define (fib-iter a b p q count) 
  (cond ((= count 0) b) 
        ((even? count) 
         (fib-iter a
                   b
                   (sum-square p q) 
                   (+ (* 2 (* p q)) (square q)) 
                   (/ count 2))) 
        (else (fib-iter (+ (* b q) (* a q) (* a p)) 
                        (+ (* b p) (* a q)) 
                        p
                        q 
                        (- count 1)))))

(fib 100000)


(define (fib n) 
  (cond ((= n 0) 0) 
        ((= n 1) 1) 
        (else (+ (fib (- n 1)) 
                 (fib (- n 2))))))

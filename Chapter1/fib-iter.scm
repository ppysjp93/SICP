
(define (fib-iter a b n) 
  (cond ((= n 0) 0)
        ((> b n) b) 
        (else (fib-iter b (+ a b) n))))

(define (fib n) 
  (fib-iter 0 1 n))

(fib 0)
(fib 1)
(fib 2)
(fib 3)
(fib 4)

; Close but no cigar, see if you can work out why the above program isn't quite
; Right at another stage. 

; Below is the definition given in the book

(define (fib n) 
  (fib-iter 1 0 n))

(define (fib-iter a b count)
    (if (= count 0)
    b
    (fib-iter (+ a b) a (- count 1))))

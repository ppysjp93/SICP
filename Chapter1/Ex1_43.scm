(define (compose f g) 
  (lambda (x) (f (g x))))

(define (repeated f n) 
  (if (= n 1) 
      f 
      (compose f (repeated f (- n 1)))))

(define (repeated f n) 
  (define (iter i result) 
    (if (= i n) 
        result 
        (iter (+ i 1) (compose result f))))
  (iter 1 f))

(define (square x) (* x x))

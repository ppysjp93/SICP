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

(define (smooth f) 
  (let ((dx 0.0001)) 
    (lambda (x) (/ (+ (f (+ x dx)) (f x) (f (- x dx))) 
                   3))))

(define (smooth-n f n) 
  ((repeated smooth n) f))

((smooth-n (lambda (x) (* x x)) 10) 10)

; Be careful about smoothing too much! 


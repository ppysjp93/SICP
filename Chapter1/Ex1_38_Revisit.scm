; Since I spent so much time making my iterative form of cont-frac I'm going
; to use it

(define (cont-frac-iter n d next combine i k) 
  (define (iter i result) 
    (if (= i 1) 
        result 
        (iter (next i) (combine (n (next i)) (d (next i)) result))))
  (iter k (/ (n k) (d k))))

(define (cont-frac n d k) 
  (cont-frac-iter n 
                 d 
                 (lambda (i) (- i 1))  
                 (lambda (n d f) (/ n (+ d f))) 
                 1 
                 k))

; We are told that we have a sequence for (d i) that does the following
; 1 2 1 1 4 1 1 6 1 1 8 ....
; We can group these values into terms of 3 numbers which is what the second
; let is doing.


(let ((i 11)) 
  (let ((n (ceiling (/ i 3)))) 
      (if (= (/ (+ i 1) 3) n) 
          (* 2 n) 
          1)))


(cont-frac (lambda (i) 1.0) 
           (lambda (i) 
             (let ((n (ceiling (/ i 3)))) 
                (if (= (/ (+ i 1) 3) n) 
                    (* 2 n) 
                    1))) 
           10)

(require racket/trace)
(trace cont-frac-iter)

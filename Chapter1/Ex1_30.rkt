(define (sum term a next b) 
  (define (iter a result) 
    (if (> a b)
        result
        (iter (next a) 
              (+ result (term a)))))
  (iter a 0))

(define (sum-cubes a b) 
  (define (inc x) (+ x 1))
  (define (cube x) (* x x x))
  (sum cube a inc b))


(require racket/trace)
(trace iter)

(sum-cubes 0 9)

; Looks like I have nailed it! 

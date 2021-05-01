(accumulate append 
            nil 
            (map (lambda (i) 
                   (map (lambda (j) (list i j)) 
                       (enumerate-interval 1 (- i 1)))) 
                 (enumerate-interval 1 n)))

(define (flatmap proc seq) 
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair) 
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair) 
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))


(define (prime-sum-pairs n) 
  (map make-pair-sum 
       (filter prime-sum? 
              (flatmap 
                (lambda (i) 
                   (map (lambda (j) (list i j)) 
                       (enumerate-interval 1 (- i 1)))) 
                (enumerate-interval 1 n)))))

; make sure you use the filter defined in the book not the in-built filter

(prime-sum-pairs 6)

(require racket/trace)

(define (permutations s) 
  (if (null? s) 
      (list nil) 
      (flatmap (lambda (x) 
                 (map (lambda (p) (cons x p)) 
                      (permutations (remove x s)))) 
               s)))

(define (remove item sequence) 
 (filter (lambda (x) (not (= x item))) 
         sequence) )









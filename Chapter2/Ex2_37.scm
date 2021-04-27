(define (accumulate op initial sequence) 
  (if (null? sequence) 
      initial
      (op (car sequence) 
          (accumulate op initial (cdr sequence)))))

(define (dot-product v w) 
  (accumulate + 0 (map * v w)))

(dot-product (list 1 2 3) (list 1 2 3))


(define (matrix-*-vector m v) 
  (map (lambda (x) (dot-product x v))  m))

(define x (list (list 1 2 3 4) 
                (list 5 6 7 8) 
                (list 9 10 11 12)))

(matrix-*-vector x (list 1 1 1 1))

(define (accumulate-n op init seqs) 
  (if (null? (car seqs)) 
      nil 
      (cons (accumulate op init (map car seqs))  
            (accumulate-n op init (map cdr seqs)))))

(define (transpose mat) 
  (accumulate-n cons nil mat))

(transpose x)

(define (matrix-*-matrix m n) 
  (let ((cols (transpose n))) 
    (map (lambda (x) (matrix-*-vector cols x)) m)))

(define m (* x x))

(matrix-*-matrix m n)

(define x (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(define (square x) (* x x))

(define (sum-odd-squares tree) 
  (cond ((null? tree) 0) 
        ((not (pair? tree)) 
         (if (odd? tree) (square tree) 0)) 
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(sum-odd-squares x)

(+ 1 9 25 49)

; Ex1_19.scm has a very efficient definition of fibonacci. 

(fib 10)

(define (even-fibs n) 
 (define (next k) 
   (if (> k n) 
       nil 
       (let ((f (fib k)) ) 
         (if (even? f) 
             (cons f (next (+ k 1))) 
             (next (+ k 1)))))) 
 (next 0))

(even-fibs 10)

(foreach (lambda (x) (newline) (display (fib x))) 
         (list 1 2 3 4 5 6 7 8 9 10))

(define (filter predicate sequence) 
  (cond ((null? sequence) nil) 
        ((predicate (car sequence)) 
         (cons (car sequence) 
              (filter predicate (cdr sequence)))) 
        (else (filter predicate (cdr sequence)))))

(define x (list 1 2 3 4 5 6 7 8 9 10))

(filter even? x)

(define (accumulate op initial sequence) 
  (if (null? sequence) 
      initial 
      (op (car sequence) (accumulate op initial (cdr sequence)))))

(accumulate + 0 x)

(define (enumerate-interval low high) 
  (if (> low high) 
      nil 
      (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 2 7)

(define (enumerate-tree tree) 
  (cond ((null? tree) nil) 
        ((not (pair? tree)) (list tree)) 
        (else (append (enumerate-tree (car tree)) 
                      (enumerate-tree (cdr tree))))))

(enumerate-tree (list 1 (list 2 (list 3 4)) 5))

(define (sum-odd-squares tree) 
  (accumulate + 
              0 
              (map square 
                   (filter odd? 
                           (enumerate-tree tree)))))

(sum-odd-squares (list 1 (list 2 (list 3 4)) 5))

(define (even-fibs n) 
  (accumulate cons 
              nil 
              (filter even? 
                      (map fib (enumerate-interval 0 n)))))

(even-fibs 10)

(define (list-fib-squares n) 
  (accumulate cons 
              nil 
              (map square 
                   (map fib (enumerate-interval 0 n)))))

(list-fib-squares 10)

(define (product-of-squares-of-odd-elements sequence) 
  (accumulate * 
              1 
              (map square 
                   (filter odd? sequence))))

(product-of-squares-of-odd-elements (list 1 2 3 4 5))






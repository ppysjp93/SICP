(define (triples n) 
  (flatmap (lambda (i) 
             (map (lambda (j) (append (list i) j))  
                  (unique-pairs (- n 1))))  
           (enumerate-interval 3 n)))

(filter (lambda (x) (> (car x) (cadr x))) (triples 5))

(define (unique-triples n) 
  (filter (lambda (x) (> (car x) (cadr x))) 
          (triples n)))

(unique-triples 5)

; Though this method works it is innefficiant but it is easy to conceptulise
; what is happening. 

(define (unique-triples n) 
  (flatmap (lambda (i) 
             (flatmap (lambda (j) 
                        (map (lambda (k) (list i j k)) 
                             (enumerate-interval 1 (- j 1)))) 
                  (enumerate-interval 1 (- i 1))))  
           (enumerate-interval 1 n)))

; this method is more efficient I think but a nested flatmap is a bit of a
; mind-bender. 

(unique-triples 5)

(define (equalTripleSum-s? s) 
  (lambda (triple) (= (accumulate + 
                             0 
                             triple) 
                      s)))

(filter (equalTripleSum-s? 10) (unique-triples 5))

(define (OrderedTriplesThatSumTo-s n s) 
  (filter (equalTripleSum-s? s) (unique-triples n)))

(OrderedTriplesThatSumTo-s 10 10)




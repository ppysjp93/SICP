(define (map p sequence) 
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(map (lambda (x) (+ x x)) (list 1 2 3))

(define (append list1 list2) 
  (if (null? list1) 
      list2 
      (cons (car list1) (append (cdr list1) list2))))

(define (append seq1 seq2) 
  (accumulate cons seq2 seq1))

(append (list 1 2 3) (list 4 5 6))

(define (length sequence) 
  (accumulate + 
              0 
              (map (lambda (x) 1) sequence)))

(length (list 1 2 3))

(define (length sequence) 
  (accumulate (lambda (x y) (+ 1 y)) 
              0 
              sequence))


(length (list 1 2 3))





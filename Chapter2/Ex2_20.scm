(define (same-parity . items) 
  (if (even? (car items)) 
      (return-even items) 
      (return-odd items)))

(define (return-even items) 
  (cond ((and (null? (cdr items)) (odd? (car items)))  nil)
        ((and (null? (cdr items)) (even? (car items))) items)
        ((even? (car items)) 
         (cons (car items) (return-even (cdr items)))) 
        ((odd? (car items)) 
         (return-even (cdr items)))))

(define (return-odd items) 
  (cond ((and (null? (cdr items)) (even? (car items)))  nil)
        ((and (null? (cdr items)) (odd? (car items))) items)
        ((odd? (car items)) 
         (cons (car items) (return-odd (cdr items)))) 
        ((even? (car items)) 
         (return-odd (cdr items)))))

(define items (list 1 2 3))

(same-parity 1 2 3)
(same-parity 1 3 4 8 9 3 2 1)
(same-parity 2 3 4 8 9 3 2 1)

; We can refactor the code so that we don't use two big blocks of code but use 
; just the one which is more satisfying and efficient and just generally better
; because it is more modularised.


(define (same-parity . items) 
  (if (even? (car items)) 
      (return-even items) 
      (return-odd items)))

(define (return-even items) 
  (return items odd? even?))

(define (return-odd items) 
  (return items even? odd?))

(define (return items proc1 proc2) 
  (cond ((and (null? (cdr items)) (proc1 (car items)))  nil)
        ((and (null? (cdr items)) (proc2 (car items))) items)
        ((proc2 (car items)) 
         (cons (car items) (return (cdr items) proc1 proc2))) 
        ((proc1 (car items)) 
         (return (cdr items) proc1 proc2))))

(same-parity 1 2 3)
(same-parity 1 3 4 8 9 3 2 1)
(same-parity 2 3 4 8 9 3 2 1)

; We can abstract this even further by saying that proc1 is not proc2 and 
; then we can reduce the number of arguments.
; I changed the name of proc2 to "condition" for readability

(define (extract items condition) 
  (return items (lambda (x) (not (condition x))) condition))

; The great thing about extract is, it has a great flexibility in usage. 
; We can use it in the same parity but we can use it for other things which is 
; very cool.

(define (same-parity . items) 
  (if (even? (car items)) 
      (extract items even?) 
      (extract items odd?)))

(same-parity 1 2 3)

; Now say we want all the numbers that are divisible by 3 from a list.

(define divisibleBy3? (lambda (x) (= (remainder x 3) 0)))

(extract (list 1 2 3 4 5 6 7 8 9) divisibleBy3?)

; extract is a powerful abstraction which may be useful in the future
; when I need certain things from lists. 

; I suppose the question is, is there a more efficient way at the lower level
; for defining how the extract operation actually works.

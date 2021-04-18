(define L1 (list 1)) 
L1
(cons (car L1) nil) ; base case for sure  0 

(define L2 (list 1 2)) 
L2
(cons (car (cdr L2)) 
      (cons (car L2) nil))

(list 2 1)

(define L3 (list 1 2 3)) 

L3

(cons (car (cdr (cdr L3)))
      (cons (car (cdr L3)) 
            (cons (car L3) nil)))

; the problem is that cons takes two arguments and I'm not sure what the 
; second argument is. 

; I'm starting to think I can't reverse a list without pouring the contents
; of the first list into the second.

(define (reverse items) 
  (if (null? (cdr items)) 
      items 
      (cons (reverse (cdr items)) (car items))))



(reverse L3)

; This is very close to the answer. 

(list 3 2 1)

; The recursive definition of reversing a list is as follows IMO: 

; The reverse of a list is where the first element of the list is appended to 
; the reverse of the sub-list. 

(define (reverse items) 
  (if (null? (cdr items)) 
      items 
      (append (reverse (cdr items)) (list (car items)))))

(reverse L3)

; Boom 

; In a way I have poured the contents of the first list into the second
; indirectly by using append.




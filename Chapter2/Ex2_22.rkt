(define square (lambda (x) (* x x)))

(require racket/trace)

(define (square-list items) 
  (trace-define (iter things answer) 
    (if (null? things) 
        answer 
        (iter (cdr things) (cons (square (car things)) answer)))) 
  (iter items nil))

(square-list (list 1 2 3 4 5))

; every value is known during an iteration. The way that this builds the 
; list is from the end backwards starting with nil as the argument

(define (square-list items) 
  (define (iter things answer) 
    (if (null? things) 
        answer 
        (iter (cdr things) 
              (cons answer 
                    (square (car things)))))) 
  (iter items nil))

; I think this will just create a strangely chaped data structure
; perhaps a tree, so it may convert the list into a sort of tree

(square-list (list 1 2 3 4 5))




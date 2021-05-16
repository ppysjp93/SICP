#lang sicp
(#%require sicp-pict)

(define (right-split painter n) 
  (if (= n 0) 
      painter 
      (let ((smaller (right-split painter (- n 1)))) 
        (beside painter (below smaller smaller)))))


(define (up-split painter n) 
  (if (= n 0) 
      painter 
      (let ((smaller (right-split painter (- n 1)))) 
        (below painter (beside smaller smaller)))))


; the trouble is getting split to recurse. 
; So I've decided to create a help function split-rec which does recurse.
; And then have it called using a lambda that takes 2 arguments


(define (split proc1 proc2) 
  (lambda (painter n) 
    (split-rec proc1 proc2 painter n)))

(define (split-rec proc1 proc2 painter n) 
    (if (= n 0) 
        painter 
        (let ((smaller (split-rec proc1 proc2 
                                  painter (- n 1)))) 
          (proc1 painter 
                 (proc2 smaller smaller)))))

; ok this is newish, as we are defining a variable that is a procedure object.
; Normally we define variables as some combination of primitives.

(define right-split2 (split beside below))
(define left-split2 (split below beside))

(paint (right-split2 einstein 3)) 
(paint (left-split2 einstein 3)) 

; cool 





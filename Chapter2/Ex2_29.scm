(define (make-mobile left right) 
  (list left right))

(define (make-branch length structure) 
  (list length structure))

(define branch1 (make-branch 1 2))
(define branch2 (make-branch 1 1))
(define branch3 (make-branch 1 branch1)) ; This is wrong, should be a mobile
(define test-mobile (make-mobile branch2 branch3))

; Part a

; A mobile is made using a list so the following selectors will suit.

(define (left-branch mobile) 
  (car mobile))

(define (right-branch mobile) 
  (car (cdr mobile)))

(define left-branch-mobile (left-branch test-mobile))
(define right-branch-mobile (right-branch test-mobile))

left-branch-mobile

right-branch-mobile


; A branch is also made using a list so the following selectors will suit 
; as well.

(define (branch-length branch) 
  (car branch))

(define (branch-structure branch) 
  (car (cdr branch)))




; Part b 

; The total weight of the mobile is the sum of the weights of all the 
; left branches and the weight of all the right branches. 
; It says the weight of a branch is the structure of a branch

(define (total-weight mobile) 
  (+ (branch-weight (left-branch mobile)) 
     (branch-weight (right-branch mobile))))

; We have now set up an abstraction barrier for the mobile "level"
; of our system
; We now consider the branch level of the system which is closer to the 
; primitive data structures we are given by Lisp
; We know that a branch is a list and we also have the fringe function 
; from exercise to flatten out a list. Once we have flattened out the 
; list we can then just sum all the values in the list if it's 
; flattened out and then we will have the branch weight. Very nice. 








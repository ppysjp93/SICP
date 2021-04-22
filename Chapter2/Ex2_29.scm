(define (make-mobile left right) 
  (list left right))

(define (make-branch length structure) 
  (list length structure))

(define branch1 (make-branch 1 1))
(define branch2 (make-branch 1 2))
(define test-mobile (make-mobile branch1 branch2))

(define branch3 (make-branch 1 test-mobile))
(define test-mobile1 (make-mobile branch1 branch3))

; Part a

; A mobile is made using a list so the following selectors will suit.

(define (left-branch mobile) 
  (car mobile))

(define (right-branch mobile) 
  (car (cdr mobile)))

(define lb-test-mobile (left-branch test-mobile))
(define rb-test-mobile (right-branch test-mobile))

lb-test-mobile
rb-test-mobile

(define lb-test-mobile1 (left-branch test-mobile1))
(define rb-test-mobile1 (right-branch test-mobile1))

lb-test-mobile1
rb-test-mobile1

; Cool so test-mobile1 is built using a branch and another mobile. 

; A branch is also made using a list so the following selectors will suit 
; as well.

(define (branch-length branch) 
  (car branch))

(define (branch-structure branch) 
  (car (cdr branch)))


(branch-length lb-test-mobile)
(branch-structure  lb-test-mobile)
(branch-length rb-test-mobile)
(branch-structure  rb-test-mobile)

(branch-length lb-test-mobile1)
(branch-structure  lb-test-mobile1)
(branch-length rb-test-mobile1)
(branch-structure  rb-test-mobile1)

; Lovely this all seems to do behave as expected. 

; Part b 
; The weight of a branch is added whenever the branch structure is 
; not a pair. Other wise we keep recursively calling through the mobile 
; until we get an accurate weight of the mobile. 

; test-mobile weight = 3 
; test-mobile1 weight = 4

; We can use the design for counting leaves to aid us in measuring the 
; weight of our mobile. 

(define (total-weight mobile) 
  (cond ((null? mobile) 0) 
        ((not (pair? mobile)) mobile)
        (else (+ (total-weight (branch-structure (left-branch mobile)))
                 (total-weight (branch-structure (right-branch mobile)))))))

(total-weight test-mobile)
(total-weight test-mobile1)

; Let's make sure this works on a much more complex mobile. 

(define branch1 (make-branch 1 test-mobile))
(define branch2 (make-branch 1 test-mobile1))
(define test-mobile2 (make-mobile branch1 branch2))

; This should have a weight of 7

(total-weight test-mobile2)

; As always I have a tendency to want to create an A and NOT B clause in my 
; definition which wasn't necessary. Trust the recursion to break the tree
; into smaller and smaller pieces!!! 

; Part c

; If both the branch length and the branch structure are not pairs we can 
; calculate the torque by multiplying these values together

(define (torque branch) 
    (* (branch-length branch)
          (total-weight (branch-structure branch))))

(define branch1 (make-branch 1 1))
(define branch2 (make-branch 1 2))

(torque branch1)
(torque branch2)

; From the definition of the torque we can create a predicate that we will
; test on all sub-mobiles of our system.  

(define (isMobileBalanced? mobile) 
  (= (torque (left-branch mobile)) 
     (torque (right-branch mobile))))

; This is for the most basic mobile where both the branch-structures for 
; the left and right branches of the mobile are not pairs. Here the weight 
; calculation is trivial. 
; When we are considering a more complex mobile the weight calculation is less
; trivial so we have to consider the total weight of the sub-mobile. 

(trace-define (balanced? mobile) 
  (cond ((not (or (pair? (branch-structure (left-branch mobile)))
                  (pair? (branch-structure (right-branch mobile))))) 
         (isMobileBalanced? mobile)) 
        (else 
          (and (balanced? (branch-structure (left-branch mobile))) 
               (balanced? (branch-structure (right-branch mobile)))))))

(define (balanced? mobile) 
  (cond ((or (not (pair? (branch-structure (left-branch mobile))))
             (not (pair? (branch-structure (right-branch mobile))))) 
         (isMobileBalanced? mobile)) 
        (else 
          (and (balanced? (branch-structure (left-branch mobile))) 
               (balanced? (branch-structure (right-branch mobile)))))))

; Test whether a theoretically symmetric balanced system returns true? 

(define branch1 (make-branch 1 1))
(define branch2 (make-branch 1 1))
(define test-mobile (make-mobile branch1 branch2))

(define branch1 (make-branch 1 1))
(define branch2 (make-branch 1 1))
(define test-mobile1 (make-mobile branch1 branch2))
 
(define branch1 (make-branch 1 test-mobile))
(define branch2 (make-branch 1 test-mobile1))
(define test-mobile2 (make-mobile branch1 branch2))

(balanced? test-mobile2)

; Test whether a theoretically symmetric imbalanced system returns false?  

(define branch1 (make-branch 1 1))
(define branch2 (make-branch 1 1))
(define test-mobile (make-mobile branch1 branch2))

(define branch1 (make-branch 1 1))
(define branch2 (make-branch 1 1.5))
(define test-mobile1 (make-mobile branch1 branch2))
 
(define branch1 (make-branch 1 test-mobile))
(define branch2 (make-branch 1 test-mobile1))
(define test-mobile2 (make-mobile branch1 branch2))

(balanced? test-mobile2)

; Test whether a theoretically assymmetric imbalanced system returns false? 

(define branch1 (make-branch 1 1))
(define branch2 (make-branch 1 1))
(define test-mobile (make-mobile branch1 branch2))
 
(define branch1 (make-branch 1 1))
(define branch2 (make-branch 1 test-mobile))
(define test-mobile1 (make-mobile branch1 branch2))

(balanced? test-mobile1)

; Test whether a theoretically assymmetric balanced system returns true? 

(define branch1 (make-branch 1 1))
(define branch2 (make-branch 2 0.5))
(define test-mobile (make-mobile branch1 branch2))
 
(define branch1 (make-branch 1.5 1))
(define branch2 (make-branch 1 test-mobile))
(define test-mobile1 (make-mobile branch1 branch2))

(balanced? test-mobile1)

; Looks like I've done it correctly! 

(define (balanced? mobile) 
  (let ((left-sub-branch 
          (branch-structure (left-branch mobile))) 
        (right-sub-branch 
          (branch-structure (right-branch mobile)))) 
   (if (or (not (pair? left-sub-branch))
               (not (pair? right-sub-branch))) 
       (isMobileBalanced? mobile) 
       (and (balanced? left-sub-branch) 
                 (balanced? right-sub-branch)))))


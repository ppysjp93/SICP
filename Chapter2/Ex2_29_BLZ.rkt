; Bill the lizard goes for a bet partitioning of the abstraction layers
; (He is more experienced, so that is fair enough).

(define (total-weight mobile) 
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (branch-weight branch) 
  (if (pair? (branch-structure branch)) 
      (total-weight (branch-structure branch)) 
      (branch-structure branch)))

; So he has two inter-woven methods that will recursively call each other.
; Very clever

(define (total-weight mobile) 
  (cond ((null? mobile) 0) 
        ((not (pair? mobile)) mobile)
        (else (+ (total-weight (branch-structure (left-branch mobile)))
                 (total-weight (branch-structure (right-branch mobile)))))))

; My method is different. It utilises the fact that a mobile is a list 
; and then finds the relevant leaves that we want, selects them and adds them 
; to the total.  

; Could I refactor mine? 

(define (total-weight mobile) 
  (+ (branch-weight (left-branch mobile)) 
     (branch-weight (right-branch mobile))))

; I wanted to make this my first step. 
; The next step was to define branch-weight which completely through me

(define (branch-weight branch) 
  (if (pair? (branch-structure branch)) 
      (total-weight (branch-structure branch)) 
      (branch-structure branch)))

; Line 37 is the move that I was missing. I forgot that you can interweave 
; recursive calls. I think this is still a very high level move. A challenge 
; to say the least. 

; Anyway at least now we have a definiton for the branch-weight so we can 
; now define something called the branch-torque 

(define (branch-torque branch) 
  (* (branch-length) 
     (branch-weight)))

; We now have to use mutual recursion again to determine whether the two branches
; are balanced. 

(define (branch-balanced? branch)
  (if (pair? (branch-structure branch)) 
      (balanced? (branch-structure branch)) 
      true))


(define (balanced? mobile) 
  (and (= (branch-torque (left-branch mobile))
          (branch-torque (right-branch mobile))) 
       (branch-balanced? (left-branch mobile)) 
       (branch-balanced? (right-branch mobile))))


(define x (list (list 1 2) (list 3 4)))

x

; recursive definition of reversing a list 
(append (reverse (cdr x)) (car x))


(define (reverse items) 
  (if (null? (cdr items)) 
      items 
      (cons (reverse (cdr items)) (car items))))

(define (deep-reverse x) 
  (cond ((null? x) null) 
        ((pair? (car x)) 
         (append (deep-reverse (cdr x)) 
                 (car x))) 
        (else (append (deep-reverse (cadr x)) 
                      (car x)))))

(pair? (car x))


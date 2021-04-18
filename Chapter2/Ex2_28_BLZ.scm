; We can use the same design that BLZ uses to define deep reverse. 

(define (append list1 list2) 
  (if (null? list1) 
      list2 
      (cons (car list1) (append (cdr list1) list2))))

; Because we are working at the leaf level and not the list level 
; fringe is going to requre cons and car to work. 


(trace-define (fringe items) 
  (cond ((null? items) null) 
        ((not (pair? (car items))) 
         (append (list (car items)) (fringe (cdr items)))) 
        ((not (pair? (cdr items))) 
         (append (fringe (car items)) (list (cdr items))))
        (else 
         (append (fringe (car items)) (fringe (cdr items))))))


; Over complicating it. I made a binary tree solution. Below is the 
; best solution.

(define (fringe tree) 
 (cond ((null? tree) null) 
       ((not (pair? tree)) (list tree)) 
       (else (append (fringe (car tree)) 
                     (fringe (cdr tree))))) )

(define x (list (list 1 2) (list 3 4)))

x

(fringe (list x x))

; This is saying that if the value of the tree is not a pair we return 
; its value as a list, otherwise we append 


(define (element-of-set? x set) 
  (cond ((null? set) false) 
        ((= x (car set)) true) 
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

; This cdrs through a list from lowest to highest. If an object is 
; equal to the first element in the list at any point then that
; means we have an element in the set. As soon as the value of 
; the object is greater that the first element in the list, we know
; that the element is not in the list.

(define (intersection-set set1 set2) 
  (if (or (null? set1) (null? set2)) 
      '() 
      (let ((x1 (car set1)) (x2 (car set2))) 
        (cond ((= x1 x2) (cons x1 
                               (intersection-set (cdr set1) 
                                                 (cdr set2)))) 
              ((< x1 x2) (intersection-set (cdr set1) set2)) 
              ((< x2 x1) (intersection-set set1 (cdr set2)))))))

; This definition for intersection-set is saying: Let's consider the 
; top elements of both lists simultaneously. If either are null then 
; intersection is null and we are at the end of either one or both of the sets. 
; If the first element in the first list is equal to the first element in the 
; second list, add it to the intersection set and let's consider the next first 
; elements of both sets. If they aren't equal there are 2 further scenarios. 
; Either the first element of the first set is less than the first element
; in the second set in which case we need to cycle further through the first
; list and keep comparing to the first element of the second list. 
; Or we have the same scenario again but vice versa. 
; We are popping two stacks simultaneously.


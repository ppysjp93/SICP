(define (reverse items) 
  (if (null? (cdr items)) 
      items 
      (append (reverse (cdr items)) (list (car items)))))

(reverse (list 1 2 3))

; In my solution I tried to use reverse to define deep reverse. 
; I shouldn't have done that. I should have just used deep-reverse
; and NOT mixed up my solution. 

(define x (list (list 1 2) (list 3 4)))

(require racket/trace)

(trace-define (deep-reverse items) 
  (cond ((null? items) null) 
        ((pair? (car items)) 
         (append (deep-reverse (cdr items)) 
                 (list (deep-reverse (car items))))) 
        (else (append (deep-reverse (cdr items)) 
                 (list (car items))))))

(deep-reverse x)

(deep-reverse (list (list 1 2) (list 3 4)))

; I think I will get better at this with experience. 

; The solution follows the basic idea of reverse. We're still appending the 
; car of the list to the reverse of its cdr, but now we check to see if 
; the car is a pair. If the car is a pair it needs reversing gain so needs
; passing through deep reverse. If not we only need to deep reverse the cdr. 


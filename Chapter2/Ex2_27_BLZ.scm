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

(deep-reverse (cons (list 1 2) (list 3 4)))

; I think I will get better at this with experience. 




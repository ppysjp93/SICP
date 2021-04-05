; The reverse of a list is the reverse of the sub-list of the list 
; with the first element appended to the end of it

(define (reverse items) 
  (if (null? (cdr items)) 
      items 
      (append (reverse (cdr items)) (list (car items)))))

(define x (list (list 1 2) (list 3 4)))

x

(car x) ; sublist (1 2)

(car (cdr x)) ; sublist (3 4)

; Method 1: Reverses the sublists and combines them into a list and then 
; reverses that comibined list

(reverse (list (reverse (car x)) 
       (reverse (car (cdr x)))))

; This is a bottom up approach where we go to the bottom and find some 
; base cases and then work our way up the list of lists.

; Method 2: Reverses the main list, it then takes the sublist of the reversed
; main list and reverses those, finally it combines those reversed sublists 
; into a single list. 

(list (reverse (car (reverse x))) 
      (reverse (car (cdr (reverse x)))))

(reverse (car (reverse x)))

; This is a top down approach, which I'm less keen on because it "feels" less
; recursive as I'm not sure what the base case would be.

; I like Method 1 because it takes fewer procedures to get the desired result
; so we want a recursive way of making that combination.

; I think we are going to need to base the design of deep-reverse on the design
; of the countleaves in some way but then modify it for my needs.

(define (count-leaves x) 
  (cond ((null? x) 0) 
        ((not (pair? x)) 1) 
        (else (+ (count-leaves (car x)) 
                 (count-leaves (cdr x))))))

; ok so at the moment deep reverse isn't recursively calling itself because
; we haven't explained how we want the base cases to work. I mean what are 
; the base cases.

; the basic problem is that I can't reverse a nil and I can't reverse a single
; value so I'm trying to premptively strike.

(trace-define (deep-reverse x) 
  (define notlist? 
    (lambda (x) (not (list? x))))
  (cond ((null? (cdr x)) x) 
        ((or (notlist? (car x)) (notlist? (car (cdr x)))) (reverse x)) 
        (else (list (deep-reverse (car (reverse x))) 
                    (deep-reverse (car (cdr (reverse x))))))))

(deep-reverse x)


; That was a particularly challenging exercise. But I just retraced my steps
; had a good long conversation with myself about the different parts 
; and then managed to get the right answer. Using the template really helped
; my understanding. It also turns out that I used method 2 to get to the solution
; which was a bit of luck! because it was just visible when I was looking at 
; how to evaluate. I wonder if method 1 would also work recursively? 
; The notes on this are definitely worth a review as they are quite extensive
; but also give insight into my thinking along the way.

; The reason method 2 works is because it matches the recursive definition
; of deep-reverse give in the question.
; The Deep Reverse of a list x is the Reverse of the list x and all sublists 
; DeepReversed as well. 

; x is the intial list to be reversed and the sublists are the cars and 
; cdrs of the list 

; Realising we wanted to stop the recursive calls were no longer lists was 
; also a key step to solving the problem

; finally know that when the (cdr x) was null? we knew we were at the end of the 
; list that needed to be reversed so we could just return the list. 

; Finally the sublist of 2 elements isn't reversing for some reason when I've 
; done some more testing. Why is this the case? (use the trace to help you of 
; course)

(define y (list 1 (list 2 3)))
y
(deep-reverse y)

(define z (list (list 2 3) 1))
z
(deep-reverse z)


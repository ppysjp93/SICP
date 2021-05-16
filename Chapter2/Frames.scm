(define (frame-coord-map frame) 
  (lambda (v) 
    (add-vect 
      (origin-frame frame) 
      (add-vect (scale-vect (xcor-vect v) 
                            (edge1-frame frame))
                (scale-vect (ycor-vect v) 
                            (edge2-frame frame))))))

; As stated in the book, frame-coord-map returns a procedure that 
; acts on a vector and returns a vector. 

(origin-frame a-frame)



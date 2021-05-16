#lang racket
(require graphics/graphics)
(open-graphics)
(define horz-vp-size 500)
(define vert-vp-size 500)
(define vp (open-viewport "A Picture Language" horz-vp-size vert-vp-size))

(define draw (draw-viewport vp))
(define (clear) ((clear-viewport vp)))
(define line (draw-line vp))

; Vector Constructor and Selectors

(define (make-vect x y)
  (list x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cadr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1)
                (xcor-vect v2))
             (- (ycor-vect v1)
                (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

;need a wrapper function so that the graphics library works with my code...
; vector are defined to be between 0 and 1 so are then scaled up to positions

(define (vector-to-posn v)
  (make-posn (* horz-vp-size (xcor-vect v))  
             (* vert-vp-size (ycor-vect v))))

(define (segments->painter segment-list)   
  (lambda (frame)     
   (for-each     
     (lambda (segment)        
      (line         
        (vector-to-posn ((frame-coord-map frame) (start-segment segment)))         
        (vector-to-posn ((frame-coord-map frame) (end-segment segment)))))      
      segment-list))) 

; Frame Constructor and Selectors

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame f)
  (car f))

(define (edge1-frame f)
  (cadr f))

(define (edge2-frame f)
  (caddr f))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

; Segment Constructor and Selectors

(define (make-segment a b) 
  (list a b))

(define (start-segment segment) 
  (car segment))

(define (end-segment segment) 
  (cadr segment))

; Solution 1 

(define v1 (make-vect 0 0))
(define v2 (make-vect 1 0))
(define v3 (make-vect 1 1))
(define v4 (make-vect 0 1))

(define bottom-segment (make-segment v1 v2))
(define right-segment (make-segment v2 v3))
(define top-segment (make-segment v3 v4))
(define left-segment (make-segment v4 v1))

(define out-line (list bottom-segment right-segment top-segment left-segment))

; Solution 2 

(define forward-slash 
  (make-segment v1 v3))

(define back-slash 
  (make-segment v2 v4))

(define X (list forward-slash back-slash))

; Solution 3 

(define (average-vec a b) 
 (define average-coor 
   (lambda (x y) (/ (+ x y) 2))) 
  (make-vect (average-coor (xcor-vect a) 
                           (xcor-vect b)) 
             (average-coor (ycor-vect a) 
                           (ycor-vect b))))


(define mid-bottom (average-vec v1 v2))
(define mid-right (average-vec v2 v3))
(define mid-top (average-vec v3 v4))
(define mid-left (average-vec v4 v1))

(define bottom-right-seg 
  (make-segment mid-bottom mid-right))

(define top-right-seg 
  (make-segment mid-right mid-top))

(define top-left-seg 
  (make-segment mid-top mid-left))

(define bottom-left-seg 
  (make-segment mid-left mid-bottom))

(define diamond (list bottom-right-seg top-right-seg 
                      top-left-seg bottom-left-seg))

; Need frame that has it's origin in the bottom left corner hence: 

(define origin (make-vect 0 1))
(define edge1 (make-vect 1 0))
(define edge2 (make-vect 0 -1))

(define frame1 (make-frame origin edge1 edge2))

;(line (vector-to-posn ((frame-coord-map frame1) v1))         
;      (vector-to-posn ((frame-coord-map frame1) v2)))

; At the moment when frame-coord-map is applied to a vector it is not producing
; the right vector. Something is wrong with the scaling

(define v6 (make-vect 0.25 0.25))

;(line (vector-to-posn ((frame-coord-map frame1) v1))         
;      (vector-to-posn ((frame-coord-map frame1) v6)))

; Outline is boring 
;((segments->painter out-line) frame1)

(define (X-painter frame) 
  ((segments->painter X) frame))

(define (diamond-painter frame) 
  ((segments->painter diamond) frame1))

(X-painter frame1)
(diamond-painter frame1)




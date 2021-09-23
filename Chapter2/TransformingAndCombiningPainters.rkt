; Notes from roughly line 118

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

; Instantiate some vectors 

(define v1 (make-vect 0.0 0.25))
(define v2 (make-vect 1.0 0.25))
(define v3 (make-vect 1.0 0.75))
(define v4 (make-vect 0.0 0.75))

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

; Instantiate a real frame

(define origin (make-vect 0 1))
(define edge1 (make-vect 1 0))
(define edge2 (make-vect 0 -1))
(define frame1 (make-frame origin edge1 edge2))

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

; Build an X painter

(define forward-slash 
  (make-segment v1 v3))

(define back-slash 
  (make-segment v2 v4))

(define X (list forward-slash back-slash))

(define (X-painter frame) 
  ((segments->painter X) frame))

;(X-painter frame1)

; Transforming and Combining Painters

(define (transform-painter painter origin corner1 corner2) 
  (lambda (frame) 
    (let ((m (frame-coord-map frame))) 
     (let ((new-origin (m origin))) 
       (painter 
         (make-frame new-origin 
                      (sub-vect (m corner1) new-origin) 
                      (sub-vect (m corner2) new-origin)))))))


(define (flip-ver painter) 
  (transform-painter painter (make-vect 0.0 1.0) 
                             (make-vect 1.0 1.0) 
                             (make-vect 0.0 0.0)))

;((flip-ver X-painter) frame1)

(define (flip-horz painter) 
  (transform-painter painter (make-vect 1.0 0.0) 
                             (make-vect 0.0 0.0) 
                             (make-vect 1.0 1.0)))

(define (shrink-to-upper-right painter) 
  (transform-painter painter (make-vect 0.5 0.5) 
                             (make-vect 1.0 0.5) 
                             (make-vect 0.5 1.0)))

;((shrink-to-upper-right X-painter) frame1)

(define (rotate90 painter) 
  (transform-painter painter (make-vect 1.0 0.0) 
                             (make-vect 1.0 1.0) 
                             (make-vect 0.0 0.0)))

;(X-painter frame1)
;((rotate90 X-painter) frame1)


(define (squash-inwards painter) 
  (transform-painter painter (make-vect 0.0 0.0) 
                             (make-vect 0.65 0.35) 
                             (make-vect 0.35 0.65)))

(define (beside painter1 painter2) 
  (let ((split-point (make-vect 0.5 0.0))) 
   (let ((paint-left 
           (transform-painter painter1 
                              (make-vect 0.0 0.0) 
                              split-point 
                              (make-vect 0.0 1.0))) 
         (paint-right 
           (transform-painter painter2 
                              split-point 
                              (make-vect 1.0 0.0) 
                              (make-vect 0.5 1.0)))) 
     (lambda (frame) 
       (paint-left frame) 
       (paint-right frame)))))

; Ex2_50.scm

(define (rotate180 painter) 
  (transform-painter painter (make-vect 1.0 1.0) 
                             (make-vect 0.0 1.0) 
                             (make-vect 1.0 0.0)))

;(X-painter frame1)
;((rotate180 X-painter) frame1)

(define (rotate270 painter) 
  (transform-painter painter (make-vect 0.0 1.0) 
                             (make-vect 0.0 0.0) 
                             (make-vect 1.0 1.0)))

;(X-painter frame1)
;((rotate270 X-painter) frame1)



; Ex2_51.scm

(define (below painter1 painter2) 
  (let ((split-point (make-vect 0.0 0.5))) 
   (let ((paint-bottom 
           (transform-painter painter1 
                              (make-vect 0.0 0.0) 
                              (make-vect 1.0 0.0)
                              split-point)) 
         (paint-top 
           (transform-painter painter2 
                              split-point 
                              (make-vect 1.0 0.5) 
                              (make-vect 0.0 1.0)))) 
     (lambda (frame) 
       (paint-bottom frame) 
       (paint-top frame)))))

;(X-painter frame1)
;((below X-painter X-painter) frame1)

; Ex2_52.scm

(define (right-split painter n) 
  (if (= n 0) 
      painter 
      (let ((smaller (right-split painter (- n 1)))) 
        (beside painter (below smaller smaller)))))

(define (up-split painter n) 
  (if (= n 0) 
      painter 
      (let ((smaller (right-split painter (- n 1)))) 
        (below painter (beside smaller smaller)))))

(define (corner-split painter n) 
 (if (= n 0) 
     painter 
     (let ((up (up-split painter (- n 1)))
           (right (right-split painter (- n 1))))
       (let ((top-left (beside up up)) 
             (bottom-right (below right right))
             (corner (corner-split painter (- n 1)))) 
         (beside (below painter top-left) 
                 (below bottom-right corner))))))


(define (square-of-four tl tr bl br) 
  (lambda (painter) 
    (let ((top (beside (tl painter) (tr painter))) 
          (bottom (beside (bl painter) (br painter)))) 
      (below bottom top))))

(define (square-limit painter n) 
  (let ((combine4 (square-of-four flip-horz identity 
                                  rotate180 flip-ver))) 
    (combine4 (corner-split painter n))))

((square-limit X-painter 3) frame1 )
;((square-limit (rotate90 X-painter) 3) frame1 )






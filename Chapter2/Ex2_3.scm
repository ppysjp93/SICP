; Note: Need to use the constructors and selectors from previous exercise.

; Top level of abstraction sp1 and sp2 are two pairs of line-segments
; that are perpendicular to each other.

(define (make-rectangle sp1 sp2) 
  (cons sp1 sp2))

(define (segment-pair1 r) (car r))
(define (segment-pair2 r) (cdr r))

; Next level of abstraction. See my notes for what a line-segment pair is.
; The important thing is that these are paralell and the same length.

(define (make-segment-pair s1 s2) 
  (cons s1 s2))

(define (segment1 sp) 
  (car sp))

(define (segment2 sp) 
  (cdr sp))

; Now we can use the selectors for a segment pair for the different methods
; of calculating the perimiters and areas.

; Let's create a rectangle from the ground up starting with some coordinates
; If we have a rectangle whose centre is at (3,3) and has a height 2 and a 
; width 4 then the points that make the rectangle are: 

(define p1 (make-point 1 2))
(define p2 (make-point 5 2))
(define p3 (make-point 5 4))
(define p4 (make-point 1 4))

; Now we need to make the segments that will make the rectangle

(define s1 (make-segment p1 p2))
(define s2 (make-segment p2 p3))
(define s3 (make-segment p3 p4))
(define s4 (make-segment p4 p1))

; Now we make the segment-pairs: sp1 are the horizontal pair and sp2 the vertical

(define sp1 (make-segment s1 s3))
(define sp2 (make-segment s2 s4))

; Finally we can now make the rectangle 

(define r1 (make-rectangle sp1 sp2))

; Now we have a rectangle from which we can calculate an area

(define (Area r) 
  (segment-pairs-area (segment-pair1 r) 
                      (segment-pair2 r)))

; Note sp1 and sp2 must be perpendicular to each-other for this to work.
; Also note how you are using the selectors defined earlier to dig deeper into 
; the layers of abstraction.

(define (segment-pairs-area horizontal-sp vertical-sp) 
  (* (segment-length (segment1 horizontal-sp)) 
     (segment-length (segment1 vertical-sp))))

; On the segment level of abstraction we need a function which will give the 
; length of segments

(define (segment-length s) 
  (distance-between (start-segment s) (end-segment s)))

; We now need to create a function that will generate the distance between 
; two points, on the point level of abstraction.

(define (distance-between p1 p2) 
  (define (square x) (* x x))
    (sqrt (+ (square (- (x-point p2) (x-point p1))) 
             (square (- (y-point p2) (y-point p1))))))

; Finally let's find the area of r1

(Area r1)

; So now let's do the same thing but for finding the perimeter of the rectangle.

(define (Perimeter r) 
  (segment-pairs-perimeter (segment-pair1 r) 
                           (segment-pair2 r)))

; Now let's define the segment-pairs-perimeter procedure 

(define (segment-pairs-perimeter horizontal-sp vertical-sp) 
  (* 2 (+ (segment-length (segment1 horizontal-sp))
          (segment-length (segment1 vertical-sp)))))

(Perimeter r1)

; Now what would be nice is a way of defining a rectangle by its 
; dimensions and by it's centre point the centre is a 
; point and dimensions are a new data type containing two parts
; what would also be nice is that it uses the code we have already made for 
; making a rectangle.

(define (make-rectangle-dim centre dimensions) 
  (let ((sp1 (make-horizontal-sp centre dimensions)) 
        (sp2 (make-vertical-sp centre dimensions))) 
    (make-rectangle sp1 sp2)))

(define (make-dimensions h w) 
  (cons h w))

(define (height d) (car d))
(define (width d) (cdr d))

; We now want to define making horizontal and vertical segment pairs 
; using the make-segment-pair we have already made.
; To do this we need to create some more methods that will allow us to create 
; horizontal segments we desire.

(define (make-horizontal-sp centre dimensions) 
  (let ((s1 (make-bottom-horizontal-s centre dimensions)) 
        (s2 (make-top-horizontal-s centre dimensions))) 
   (make-segment-pair s1 s2)))

(define (make-vertical-sp centre dimensions) 
  (let ((s1 (make-left-vertical-s centre dimensions)) 
        (s2 (make-right-vertical-s centre dimensions))) 
   (make-segment-pair s1 s2)))

; Now we have 4 more new functions to create which pass centre and dimensions
; as the arguments to the function. Again we want to use the make-segment 
; function that we have already created.
; We are now finally at level where can start making some concretions because we 
; can define points using centre and dimensions. Up to this point we have been
; been unable to do this.

(define (make-bottom-horizontal-s centre dimensions) 
  (let ((p1 (make-point (- (x-point centre) (/ (width dimensions) 2)) 
                        (- (y-point centre) (/ (height dimensions) 2)))) 
        (p2 (make-point (+ (x-point centre) (/ (width dimensions) 2)) 
                        (- (y-point centre) (/ (height dimensions) 2))))) 
    (make-segment p1 p2)))

; Once you define the first method it's easy to make the others because
; you can just copy the template.

(define (make-top-horizontal-s centre dimensions) 
  (let ((p1 (make-point (- (x-point centre) (/ (width dimensions) 2)) 
                        (+ (y-point centre) (/ (height dimensions) 2)))) 
        (p2 (make-point (+ (x-point centre) (/ (width dimensions) 2)) 
                        (+ (y-point centre) (/ (height dimensions) 2))))) 
    (make-segment p1 p2)))


(define (make-left-vertical-s centre dimensions) 
  (let ((p1 (make-point (+ (x-point centre) (/ (width dimensions) 2)) 
                        (- (y-point centre) (/ (height dimensions) 2)))) 
        (p2 (make-point (+ (x-point centre) (/ (width dimensions) 2)) 
                        (+ (y-point centre) (/ (height dimensions) 2))))) 
    (make-segment p1 p2)))


(define (make-right-vertical-s centre dimensions) 
  (let ((p1 (make-point (- (x-point centre) (/ (width dimensions) 2)) 
                        (- (y-point centre) (/ (height dimensions) 2)))) 
        (p2 (make-point (- (x-point centre) (/ (width dimensions) 2)) 
                        (+ (y-point centre) (/ (height dimensions) 2))))) 
    (make-segment p1 p2)))


(define c (make-point 3 3))
(define d (make-dimensions 2 4))

(define r2 (make-rectangle-dim  c d))

(Area r2)
(Perimeter r2)

; Boom.

; Now that we define the rectangle from the centre point and its dimensions
; we may wish to know the co-ordinates of one of the corners of the Rectangle.
; We could now create a method at this level of abrastraction that finds the 
; bottom left of a rectangle. We may also wish to rotate the rectangle by 
; some angle, we could also create a procedure for doing this also.

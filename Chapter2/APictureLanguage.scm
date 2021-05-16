#lang sicp
(#%require sicp-pict)

; Painters means of combination

;beside: first image is left of the second image
;below: first image is below the second image
;flip-vert: produces the same image but upside-down
;flip-horiz: produces the same painter left-right reversed.

(define einstein2 (beside einstein (flip-vert einstein)))
(define einstein4 (below einstein2 einstein2))

(paint einstein4)

(define (flipped-pairs painter) 
  (let ((painter2 (beside painter (flip-vert painter)))) 
    (below painter2 painter2)))

(paint (flipped-pairs einstein))

(define (right-split painter n) 
  (if (= n 0) 
      painter 
      (let ((smaller (right-split painter (- n 1)))) 
        (beside painter (below smaller smaller)))))

(paint (right-split einstein 3))

(define (up-split painter n) 
  (if (= n 0) 
      painter 
      (let ((smaller (right-split painter (- n 1)))) 
        (below painter (beside smaller smaller)))))

(paint (up-split einstein 3))

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

(paint (corner-split einstein 3))

(define (square-limit painter n) 
  (let ((quarter (corner-split painter n))) 
    (let ((half (beside (flip-horiz quarter) quarter))) 
      (below (flip-vert half) half))))

(paint (square-limit einstein 3)) 

(define (square-of-four tl tr bl br) 
  (lambda (painter) 
    (let ((top (beside (tl painter) (tr painter))) 
          (bottom (beside (bl painter) (br painter)))) 
      (below bottom top))))

(paint ((square-of-four (lambda (painter) (up-split painter 3)) 
                        (lambda (painter) (up-split painter 3))
                        (lambda (painter) (up-split painter 3))  
                        (lambda (painter) (up-split painter 3))) 
        einstein))

(define (flipped-pairs2 painter) 
 (let ((combine4 (square-of-four identity 
                                 flip-vert 
                                 identity
                                 flip-vert))) 
   (combine4 painter)))

(paint (flipped-pairs2 einstein))











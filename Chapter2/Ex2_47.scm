; Solution one

(define (make-frame origin edge1 edge2) 
  (list origin edge1 edge2))

(define (origin-frame frame) 
  (car frame))

(define (edge1-frame frame) 
  (car (cdr frame)))

(define (edge2-frame frame) 
  (car (cdr (cdr frame))))

; Test

(define frame (list 1 2 3))
(origin-frame frame)
(edge1-frame frame)
(edge2-frame frame)

; Solution 2

(define (make-frame origin edge1 edge2) 
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame) 
  (car frame))

(define (edge1-frame frame) 
  (car (cdr frame)))

(define (edge2-frame frame) 
  (cdr (cdr frame)))
 
(define frame (cons 1 (cons 2 3)))
(origin-frame frame)
(edge1-frame frame)
(edge2-frame frame)



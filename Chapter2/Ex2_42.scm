; General helper functions for solution

(define (true-list? bool-sequence) 
  (accumulate (lambda (x y) (and (eq? x true) y)) 
              true 
              bool-sequence)) 

(define (filter predicate sequence) 
  (cond ((null? sequence) nil) 
        ((predicate (car sequence)) 
         (cons (car sequence) 
              (filter predicate (cdr sequence)))) 
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence) 
  (if (null? sequence) 
      initial 
      (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq) 
  (accumulate append nil (map proc seq)))

(define (enumerate-interval low high) 
  (if (> low high) 
      nil 
      (cons low (enumerate-interval (+ low 1) high))))

(define (display-set set-of-positions) 
  (if (null? set-of-positions) 
      nil
      (and (display (car set-of-positions)) 
           (newline) 
           (display-set (cdr set-of-positions)))))

(define (list-ref items n) 
  (if (= n 0) 
      (car items) 
      (list-ref (cdr items) (- n 1))))

; Setting up abstraction barrier for postion 

(define (position col row) 
  (list col row))

(define (col position) 
  (car position))

(define (row position) 
  (cadr position))



; Solution for adjoin-position

(define (adjoin-position new-row k rest-of-queens) 
  (append rest-of-queens (list (position k new-row))))

(define (queens board-size) 
  (define (queen-cols k) 
    (if (= k 0) 
        (list nil) 
        (flatmap 
            (lambda (rest-of-queens) 
              (map (lambda (new-row) 
                   (adjoin-position new-row k rest-of-queens)) 
                 (enumerate-interval 1 board-size))) 
            (queen-cols (- k 1))))) 
  (queen-cols board-size))

(define k 8)
(define set-of-positions (queens k))  
(define positions (car set-of-positions))
(define kth-queen (list-ref positions (- k 1)))

(define other-queens (remove kth-queen positions))

(display-set set-of-positions)
(display positions)
(display other-queens)



; Soluton for safe?

; check to see if every queen relative to the kth-queen is safe.
; This is done by checking every other queen relative to the kth-queen
; and seeing if it makes a safe-coordinate pair. The map produces a 
; boolean list. If all the values in this list are true then true-list?
; will return true, otherwise it will return false.


(define (equal-position? position kth-queen) 
  (and (= (row position) (row kth-queen)) 
       (= (col position) (col kth-queen))))

(define (remove kth-queen positions) 
 (filter (lambda (position) 
           (not (equal-position? position kth-queen))) 
         positions))

(define (safe? k positions) 
  (let ((kth-queen (list-ref positions (- k 1))))  
  (true-list? (map (lambda (x) (safe-coordinate-pair? x kth-queen)) 
                  (remove kth-queen positions)))))

(define (queens board-size) 
  (define (queen-cols k) 
    (if (= k 0) 
        (list nil) 
        (filter 
          (lambda (positions) (safe? k positions)) 
          (flatmap 
            (lambda (rest-of-queens) 
              (map (lambda (new-row) 
                   (adjoin-position new-row k rest-of-queens)) 
                 (enumerate-interval 1 board-size))) 
            (queen-cols (- k 1)))))) 
  (queen-cols board-size))

(display-set (queens 8))

; Helper Functions for safe-coordinate-pair?

(define (same-col? p1 p2) 
  (= (col p1) (col p2)))


(define (same-row? p1 p2) 
  (= (row p1) (row p2)))

(define (gradient p1 p2) 
  (if (equal-position? p1 p2) 
      (error "Both Positions Overlap") 
      (/ (- (row p2) 
            (row p1)) 
         (- (col p2) 
            (col p1))))) 

(define (same-diag? p1 p2) 
  (= (abs (gradient p1 p2)) 1))

(same-diag? (list 1 1) (list 5 5))
(same-diag? (list 1 1) (list 5 4))
(same-diag? (list 5 5) (list 1 1))
(same-diag? (list 5 4) (list 1 1))
(same-diag? (list 5 5) (list 5 5))

; Using De'Morgans Law

(define (safe-coordinate-pair? p1 p2) 
  (not (or (same-col? p1 p2) 
           (same-row? p1 p2) 
           (same-diag? p1 p2))))


(safe-coordinate-pair? (list 1 1) (list 2 3))
(safe-coordinate-pair? (list 1 1) (list 1 5))
(safe-coordinate-pair? (list 1 1) (list 5 1))
(safe-coordinate-pair? (list 1 1) (list 5 5))

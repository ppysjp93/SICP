(define (make-leaf symbol weight) 
  (list 'leaf symbol weight))

(define (leaf? object) 
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right) 
  (list left 
        right 
        (append (symbols left) (symbols right)) 
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

; The procedures symbols and weight need to do something slightly different
; depending on whether they are called with a leaf or a general tree. 

(define (symbols tree) 
  (if (leaf? tree) 
      (list (symbol-leaf tree)) 
      (caddr tree)))

(define (weight tree) 
  (if (leaf? tree) 
      (weight-leaf tree) 
      (cadddr tree)))

(define A (make-leaf 'A 8))
(define B (make-leaf 'B 3))
(define C (make-leaf 'C 1))
(define D (make-leaf 'D 1))
(define E (make-leaf 'E 1))
(define F (make-leaf 'F 1))
(define G (make-leaf 'G 1))
(define H (make-leaf 'H 1))

(define EF (make-code-tree E F))
(define CD (make-code-tree C D))
(define GH (make-code-tree G H))

(define EFGH (make-code-tree EF GH))
(define BCD (make-code-tree B CD))

(define BCDEFGH (make-code-tree BCD EFGH))

(define ABCDEFGH (make-code-tree A BCDEFGH))

ABCDEFGH

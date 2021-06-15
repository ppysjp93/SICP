(define (decode bits tree) 
  (define (decode-1 bits current-branch) 
    (if (null? bits) 
        '() 
        (let ((next-branch 
                (choose-branch (car bits) current-branch))) 
          (if (leaf? next-branch) 
              (cons (symbol-leaf next-branch) 
                    (decode-1 (cdr bits) tree)) 
              (decode-1 (cdr bits) next-branch))))) 
  (decode-1 bits tree))


(define (choose-branch bit branch) 
  (cond ((= bit 0) (left-branch branch)) 
        ((= bit 1) (right-branch branch)) 
        (else (error "bad bit --  CHOOSE-BRANCH" bit))))

; Take ABCDEFGH defined in HuffmanEncodingTree.scm as the "tree"

(define (adjoin-set x set) 
  (cond ((null? set) (list x)) 
        ((< (weight x) (weight (car set))) (cons x set)) 
        (else (cons (car set) 
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs) 
  (if (null? pairs) 
      '() 
      (let ((pair (car pairs))) 
        (adjoin-set (make-leaf (car pair) 
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))



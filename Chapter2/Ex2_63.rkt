(define leaf1 (make-tree 1 '() '()))
(define leaf2 (make-tree 5 '() '()))
(define leaf3 (make-tree 11 '() '()))
(define leaf4 (make-tree 7 '() '()))

(display leaf1)
(display leaf2)
(display leaf3)
(display leaf4)

(define sub-tree1 (make-tree 3 leaf1 leaf2))
(define sub-tree2 (make-tree 9 '() leaf3))
(define sub-tree3 (make-tree 7 leaf2 sub-tree2))
(define sub-tree4 (make-tree 3 leaf1 '()))
(define sub-tree5 (make-tree 3 leaf1 '()))
(define sub-tree6 (make-tree 9 leaf4 leaf3))

(display sub-tree1)
(display sub-tree2)
(display sub-tree3)
(display sub-tree4)
(display sub-tree5)

(define tree1 (make-tree 7 sub-tree1 sub-tree2))
(define tree2 (make-tree 3 leaf1 sub-tree3))
(define tree3 (make-tree 5 sub-tree5 sub-tree6))

(display tree1)

(display tree2)
(display tree3)


(require racket/trace)

(trace-define (tree->list-1 tree) 
  (if (null? tree) 
      '() 
      (append (tree->list-1 (left-branch tree)) 
              (cons (entry tree) 
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree) 
  (trace-define (copy-to-list tree result-list) 
    (if (null? tree) 
        result-list 
        (copy-to-list (left-branch tree) 
                      (cons (entry tree) 
                            (copy-to-list (right-branch tree) 
                                          result-list))))) 
  (copy-to-list tree '()))
 
(tree->list-1 tree1)

(tree->list-2 tree1)

(tree->list-1 tree2)
(tree->list-2 tree2)

(tree->list-1 tree3)
(tree->list-2 tree3)

; Both methods produce the same lists. 
; The first method produces strange situatations at leaves like below
; which constructs the correct leaf but in an innefficient manner

(append null (cons 1 null)) -> (cons 1 null)

(length (list 1 3 5 9 11))

(quotient (- 2 1) 2)





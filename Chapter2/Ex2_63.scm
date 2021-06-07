(define (tree->list-1 tree) 
  (if (null? tree) 
      '() 
      (append (tree->list-1 (left-branch tree)) 
              (cons (entry tree) 
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree) 
  (define (copy-to-list tree result-list) 
    (if (null? tree) 
        result-list 
        (copy-to-list (left-branch tree) 
                      (cons (entry tree) 
                            (copy-to-list (right-branch tree) 
                                          result-list))))) 
  (copy-to-list tree '()))


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
(define sub-tree5 (make-tree 9 leaf4 leaf3))


(display sub-tree1)
(display sub-tree2)

(display sub-tree3)

(display sub-tree4)
(display sub-tree5)



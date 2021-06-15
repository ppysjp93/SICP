(define (generate-huffman-tree pairs) 
  (successive-merge (make-leaf-set pairs)))

(define pairs (list (list 'A 4) (list 'B 2) (list 'C 1) (list 'D 1)))

(define leaf-set (make-leaf-set pairs))


(define (successive-merge leaf-set) 
  (if (= (length leaf-set) 1) 
      (car leaf-set) 
      (let ((first (car leaf-set)) 
            (second (cadr leaf-set))
            (rest (cddr leaf-set))) 
        (successive-merge (adjoin-set (make-code-tree first 
                                                      second)
                                      rest)))))

(successive-merge leaf-set)

; Makes a lot of sense. I think I would have gotten there in the end







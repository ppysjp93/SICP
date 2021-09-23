(define sample-tree 
  (make-code-tree (make-leaf 'A 4) 
                  (make-code-tree 
                    (make-leaf 'B 2) 
                    (make-code-tree (make-leaf 'D 1) 
                                    (make-leaf 'C 1)))))

sample-tree

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

sample-message

(decode sample-message sample-tree)

; The decoded sample message is: 
; ADABBCA
; Huffman tree is drawn in book. 


(define (encode message tree) 
  (if (null? message) 
      '() 
      (append (encode-symbol (car message) tree) 
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree) 
  (cond ((eq? symbol 'A) '(0)) 
        ((eq? symbol 'B) '(1 0)) 
        ((eq? symbol 'C) '(1 1 1))
        ((eq? symbol 'D) '(1 1 0))
        (else (error "symbol not in tree!"))))

(define (encode-symbol symbol tree) 
  (cond ((and (leaf? tree) (not (eq? symbol (symbol-leaf tree)))) 
         (error "Symbol not in tree!")) 
        ((leaf? tree) null) 
        (else (let ((left (left-branch tree)) 
                    (right (right-branch tree))) 
                (if (eq? symbol (car (symbols tree))) 
                    (append '(0) (encode-symbol symbol left)) 
                    (append '(1) (encode-symbol symbol right)))))))


(encode-symbol 'A sample-tree)
(encode-symbol 'B sample-tree)
(encode-symbol 'C sample-tree)
(encode-symbol 'D sample-tree)
(encode-symbol 'E sample-tree)

(define sample-message2 '(A D A B B C A))

(encode sample-message2 sample-tree)



(define alphabet (list (list 'A 16) 
                       (list 'B 8)
                       (list 'C 4)
                       (list 'D 2)
                       (list 'E 1)))

(display alphabet)

(define huffman-tree (generate-huffman-tree alphabet))

(display huffman-tree)

(define alphabet (list (list 'A 512)
                       (list 'B 256)
                       (list 'C 128)
                       (list 'D 64)
                       (list 'E 32)
                       (list 'F 16) 
                       (list 'G 8)
                       (list 'H 4)
                       (list 'I 2)
                       (list 'J 1)))

(define huffman-tree (generate-huffman-tree alphabet))

(display huffman-tree)


(define alphabet (list (list 'A 2) 
                       (list 'BOOM 1)
                       (list 'GET 2)
                       (list 'JOB 2)
                       (list 'NA 16)
                       (list 'SHA 3)
                       (list 'YIP 9)
                       (list 'WAH 1)))

(display alphabet)

(define huffman-tree (generate-huffman-tree alphabet))

huffman-tree

; Draw tree out in book

; From the tree we must create encode symbol

(define (encode-symbol symbol tree) 
  (cond ((eq? symbol 'A) '(1 1 0 0)) 
        ((eq? symbol 'BOOM) '(1 1 0 1 1)) 
        ((eq? symbol 'GET) '(1 1 1 1 1))
        ((eq? symbol 'JOB) '(1 1 1 1 0))
        ((eq? symbol 'NA) '(0))
        ((eq? symbol 'SHA) '(1 1 1 0))
        ((eq? symbol 'YIP) '(1 0))
        ((eq? symbol 'WAH) '(1 1 0 1 0))
        (else (error "symbol not in tree!"))))

(encode (list 'GET 'A 'JOB) huffman-tree)

(encode (list 'SHA 'NA 'NA 'NA 'NA 'NA 'NA 'NA 'NA) huffman-tree)

(encode (list 'GET 'A 'JOB) huffman-tree)

(encode (list 'SHA 'NA 'NA 'NA 'NA 'NA 'NA 'NA 'NA) huffman-tree)

(encode (list 'WAH 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP) huffman-tree)

(encode (list 'SHA 'BOOM) huffman-tree)

; I think the number of bits required is the frequency multiplied by 
; the encode symbold length for each symbol and then summed. 

; 2 * 4 = 8     8
; 1 * 5 = 5     13
; 2 * 5 = 10    23
; 2 * 5 = 10    33
; 16 * 1 = 16   49
; 3 * 4 = 12    61
; 2 * 2 = 4     65
; 1 * 5 = 5     70

; 70 bits in total

; If we used a fixed length 8 symbol alphabet then the number of bits would 
; be 32 * 3 = 96





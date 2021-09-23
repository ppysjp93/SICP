(define (filter predicate sequence) 
  (accumulate (lambda (x y) (if (predicate x)
                                (cons x y) 
                                y)) 
              nil 
              sequence))

(filter odd? (list 1 2 3 4))

; I think we need to create a different kind of filter which is similar
; to the above. 

(define x (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

x


(define (accumulate-n op init seqs) 
  (if (null? (car seqs)) 
      nil 
      (cons (accumulate op init (map car seqs))  
            (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 x)








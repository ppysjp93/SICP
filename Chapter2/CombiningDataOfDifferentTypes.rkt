;; to be included in the complex package

(define (add-complex-to-schemenum z x) 
  (make-from-real-imag (+ (real-part z) x) 
                       (imag-part z)))

(put 'add '(complex schemenumber) 
     (lambda (z x) (tag (add-complex-to-schemenum z x))))

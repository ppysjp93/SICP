#lang sicp

; Division-1 chooses to construct the personel data as a list containing
; 3 elements, a name, an address and a salary. A file is then constructed
; as a list of personnel data. To look-up the personnel data, Divison1
; chooses to use the name of the employee as the key for selecting it.

(define (get-name record) 
  (car record))

(define (get-address record) 
  (cadr record))

(define (get-salary record) 
  (caddr record))

(define (make-record name address salary) 
  (list name address salary))

(define record1 (make-record 'Sam 'Parnell 100))
(define record2 (make-record 'Tom 'Edward 1000))
(define record3 (make-record 'Rob 'Hanbury 500))
(define record4 (make-record 'Andy 'Springwood 500))

(define (make-file . records) 
  (cons (car records) (cdr records)))

(define file1 (list record1 record2 record3 record4))
(define file2 (make-file record1 record2 record3 record4))

; make-file test: make sure file1 and file2 have the same data structure 

file1
file2

(define (get-record key file) 
  (cond ((null? file) (error "Employee not in file")) 
        ((eq? key (get-name (car file))) 
         (car file)) 
        (else (get-record key (cdr file)))))

; look-up-employee test cases

;(get-record 'Dom '())
(get-record 'Sam file1)
(get-record 'Rob file1)


(define file2 ((lambda records (make-file records)) record1 record2 record3))

file2







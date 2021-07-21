#lang sicp

(#%require (only racket/base error))
(#%require (only racket/base make-hash))
(#%require (only racket/base hash-set!))
(#%require (only racket/base hash-ref))

; table set up

(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) '()))

; data tagging set up

(define (attach-tag type-tag contents) 
  (cons type-tag contents))

(define (type-tag datum) 
  (if (pair? datum) 
      (car datum) 
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum) 
  (if (pair? datum) 
    (cdr datum) 
    (error "Bad tagged datum -- CONTENTS" datum)))

(define (apply-generic op . args) 
  (let ((type-tags (map type-tag args))) 
    (let ((proc (get op type-tags))) 
      (if proc 
          (apply proc (map contents args)) 
          (error 
            "No method for these types -- APPLY-GENERERIC" 
            (list op type-tags))))))

(define (install-Div1-Package) 
  (define (get-name record) 
  (car record))
  (define (get-address record) 
    (cadr record))
  (define (get-salary record) 
    (caddr record))
  (define (make-record name address salary) 
    (list name address salary))
  (define (get-record name file) 
    (cond ((null? file) (error "Employee not in file")) 
          ((eq? name (get-name (car file))) 
           (car file)) 
          (else (get-record name (cdr file)))))
  (define (make-file . records) 
    records)
  ;interface to the rest of the system
 (define (tag x) (attach-tag 'Div1 x)) 
 (put 'get-name '(Div1) get-name)
 (put 'get-address '(Div1) get-address)
 (put 'get-salary 'Div1 get-salary)
 (put 'make-record 'Div1 
      (lambda (name address salary) 
        (make-record name address salary)))
 (put 'get-record 'Div1 
      (lambda (name file) 
        (get-record name file)))
 (put 'make-file 'Div1 
      (lambda args 
        (tag (make-file args)))))

(install-Div1-Package)

(define (make-record name address salary) 
  ((get 'make-record 'Div1) name address salary))

(define record1 (make-record 'Sam 'Parnell 100))
(define record2 (make-record 'Tom 'Edward 1000))
(define record3 (make-record 'Rob 'Hanbury 500))
(define record4 (make-record 'Andy 'Springwood 500))

(define (make-file . records) 
  (apply (get 'make-file 'Div1) records))

(define file1 (make-file record1 record2 record3 record4))

file1

; Above is how one division could implement the record-system

(define (get-division generic-file) 
  (car generic-file))

(define (get-records generic-file) 
  (cadr generic-file))

; Part A

(define (get-record name generic-file) 
  (let ((division (get-division generic-file))
        (records (get-records generic-file))) 
    ((get 'get-record division) name records)))

(get-record 'Andy file1)

; Part B

(define (get-salary name generic-file) 
  (let ((division (get-division generic-file))
        (records (get-records generic-file))) 
   (let ((record ((get 'get-record division) name records))) 
     ((get 'get-salary division) record))))

(get-salary 'Andy file1)

; Part C

(define (first-file generic-files) 
  (car generic-files))

(define (rest-of-files generic-files) 
  (cdr generic-files))

(define (find-employee-record name generic-files) 
  (let ((generic-file 
          (first-file generic-files)) 
        (remainding-files 
          (rest-of-files generic-files))) 
    (if (get-record name generic-file) 
        (get-record name generic-file) 
        (find-employee-record name remainding-files))))

(define files (list file1))

(find-employee-record 'Andy files)

; Part C

; Another table needs setting up with the relevant methods implemented 
; correctly for the records
; At some point try implementing a different design for the data coming 
; in from a different division.



















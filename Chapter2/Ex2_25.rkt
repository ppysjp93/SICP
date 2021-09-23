(define l1 (list  1 3 (list 5 7) 9))
l1

(car (cdr (car (cdr (cdr l1))))) 

(define l2 (list (list 7)))
l2

(car (car l2))

(define l3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
l3

(cdr l3)

(car (cdr l3))

(cdr (car (cdr l3))) 

(car (cdr (car (cdr l3))))

(cdr (car (cdr (car (cdr l3)))))

(car (cdr (car (cdr (car (cdr l3))))))

(cdr (car (cdr (car (cdr (car (cdr l3)))))))

(car (cdr (car (cdr (car (cdr (car (cdr l3)))))))) 

(cdr (car (cdr (car (cdr (car (cdr (car (cdr l3)))))))))

(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l3))))))))))

(cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l3)))))))))))

(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l3)))))))))))) 


(list 1)

(cons 1 nil)

(list 1 (list 1) 3)

(list 1 1 3)


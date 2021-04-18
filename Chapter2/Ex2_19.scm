(define (count-change amount) 
  (cc amount us-coins))

(define us-coins (list 50 25 10 5 1))

; values of coins is a list

(define (cc amount values-of-coins) 
  (cond ((= amount 0) 1) 
        ((or (< amount 0) (null? values-of-coins)) 0) 
        (else (+ (cc amount 
                     (cdr values-of-coins)) 
                 (cc (- amount 
                        (car values-of-coins)) 
                     values-of-coins)))))

(count-change 100)

; using a list data-structure here is clearly much more efficient use of code

(define uk-coins (list 20 50 5 10 2 1))

(cc 100 uk-coins)

; Solution to question is as follows

(define (cc amount coin-values) 
  (cond ((= amount 0) 1) 
        ((or (< amount 0) (no-more? coin-values)) 0) 
        (else (+ (cc amount 
                     (except-first-denomination coin-values)) 
                 (cc (- amount 
                        (first-denomination coin-values)) coin-values)))))

(define (no-more? coin-values) 
  (null? coin-values))

(define (except-first-denomination coin-values) 
  (cdr coin-values))

(define (first-denomination coin-values) 
  (car coin-values))

(cc 100 uk-coins)


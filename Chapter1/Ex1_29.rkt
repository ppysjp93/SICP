(define (sum term a next b) 
  (if (> a b)
      0
      (+ (term a) 
         (sum term (next a) next b))))

(define (h a b n) 
  (/ (- b a) n))

(h 1.0 2.0 1000)

(define (add-h x) 
  (+ x h))

(define (interval f x h) 
  (+ (* 2 (f x)) 
     (* 4 (f (+ x (/ h 2)))) 
     (f (+ x h))))


(define (simpson f a b n)
  (define (h a b n) 
    (/ (- b a) n))
  (define (add-h x) 
    (+ x h))
  (define (interval f x h) 
    (+ (* 2 (f x)) 
       (* 4 (f (+ x (/ h 2)))) 
       (f (+ x h))))
 (* (/ h 3) (sum interval a add-h b)))

(define (cube x) 
  (* x x x))

(require racket/trace)
(trace simpson)

(simpson cube 0.0 1.0 100)

(define (integral f a b dx) 
  (define (add-dx x) (+ x dx)) 
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

; The difference between mine and the integral is that I have 
; f directly acting on in my definition.
; When I look at the example f is acting on the next a. 


(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (add-h x) 
    (+ x h))
  (define (interval f x) 
    (+ (* 2 (f x)) 
       (* 4 (f (+ x (/ h 2)))) 
       (f (+ x h))))
 (* (/ h 3) (sum interval a add-h b)))

; So my current method for varying f is NOT the way to go. 
; The integral method worked because we were summing where the 
; term f didn't change. The problem we have got is that the f term doesn't 
; factor out as nicely as it does for the poorer approximation. 
; To deal with this I created the term above called interval but it's not 
; implemented correctly I don't think.

; I mean how else would you apply the varying f to a? 
 
(define (even? x) 
  (= (remainder x 2) 0))

(define h (/ (- b a) n))

(define (y k) 
  (f (+ a (* k h))))

(define (simpson-term k) 
  (* (cond ((or (= k 0) (= k n)) 1) 
           ((even? k) 2) 
           (else 4)) 
     (y k)))


(define (inc x) (+ x 1))

(define (simpson f a b n)
  (define (even? x) 
    (= (remainder x 2) 0))
  (define (inc x) (+ x 1))
  (define h (/ (- b a) n))
  (define (y k) 
    (f (+ a (* k h))))
  (define (simpson-term k) 
    (* (cond ((or (= k 0) (= k n)) 1) 
             ((even? k) 2) 
             (else 4)) 
       (y k))) 
  (/ (* h (sum simpson-term 0 inc n)) 3))

(trace sum)
(simpson cube 0 1 100.0)




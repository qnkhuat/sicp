; Buildling abstractions with Data
(define f "lec2.scm")
(define l load)

(l "util.scm")

; Greaetest common divisors
(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (make-rat n d)
  (define (norm-sign n d)
    (cond ((and (< n 0) (< d 0)) +)
          ((and (> n 0) (> d 0)) +)
          (else -)))
  (let ((g (abs (gcd n d))))
    (cons (/ ((norm-sign n d) (abs n)) g) (/ (abs d) g))))

(define (numer x) (car x)); Return its numerator
(define (denom x) (cdr x)) ; Return its denominator

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))


(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (display (numer x))
  (display '/)
  (display (denom x))
  (newline)
  )

(define one-half (make-rat 1 2))
(define quater (make-rat 1 4))
(define one-third (make-rat 1 3))
(define minus-one-third (make-rat -1 3))

; E.x: 2.2
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define (midpoint-segment s)
  (let (
        (p1 (start-segment s))
        (p2 (end-segment s))
        (mid-x (avg (x-point p1 ) (x-point p2)))
        (mid-y (avg (y-point p1 ) (y-point p2)))
        )
    (make-point mid-x mid-y)
    ))


(define p1 (make-point 0 0))
(define p2 (make-point 10 10))
(define s (make-segment p1 p2))

(print-point (midpoint-segment s))



; Even though cons, car, cdr are primivites but we can define them as
(define (cons x y)
  (define dispatch m)
  (cond ((= m 0) x)
        ((= m 1) y)
        (else (error "Arguent not 0 or 1 -- CONS" m))
        )
  dispatch)
(define (car z) (z 0))
(define (cdr z) (z 1))



; E.x: 2.4
;(define (cons x y)
;  (lambda (m) (m x y)))
;
;(define (car z)
;  (z (lambda (p q) p)))
;
;(define (cdr z)
;  (z (lambda (p q) q)))
;
;(car (cons 3 4))


; E.x: 2.5
(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (count-till-indivisible count num dividend)
  (if (= (remainder num dividend) 0)
    (count-till-indivisible (+ 1 count) (/ num dividend) dividend)
    count))

;(define (car z)
;  (count-till-indivisible 0 z 2))
;
;(define (cdr z)
;  (count-till-indivisible 0 z 3))


; E.x: 2.6

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
;;; To be continued



; 2.1.4 Extended Exercise: Interval Arithmetic
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (make-interval x y)
  (cons x y))


(define (lower-bound i) (car (i)))
(define (upper i) (cdr (i)))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

; E.x: 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

; 2.2 Hierarchical Data and the Closure Property
(define one-to-four (list 1 2 3 4)) ; list is a chains of cons
;(display (list-ref one-to-four 3)) ; get the 3rd elem. It's also built-in
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

;(display (length one-to-for)) ; length is a built-in operation
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
















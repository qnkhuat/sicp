; Run scheme by M-x run-scheme
; Fib example
(define (fib n)
  (if (< n 2)
      1
      (+ (fib (- n 1)) (fib (- n 2)))))
(fib 5)
; press C-x C-e to evalute this

; E.g 129 Simpson's Rule
(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))


(integral cube 0 1 0.01)
(integral cube 0 1 0.001)


(define (simpson f a b n)
  (define (get-coff k)
    (cond ((or (= k 0) (= k n)) (1))
	  ((= (remainder k 2) 0) (2))
	  ((= (remainder k 2) 1) (4))
     ))
  (define iter (h k)
    (if (> k n)
	0
	(+ (* (get-coff k) (f (+ a (* k h))))
	   (iter (h) (+ k 1))
	   )
    ))
  (define h (/ (- b a) n))
  (iter (h) 0)
  )
(simpson cube 0 1 100)

































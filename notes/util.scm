; Utilities
(define (abs x)
  (if (< x 0) (- x) x))

(define (avg a b) (/ (+ a b) 2))

(define nil '())


(define (fib n)
  (define (fib-iter k n1 n2)
    (if (= k n)
      n2
    (fib-iter (+ 1 k) n2 (+ n1 n2))))
  (fib-iter 1 1 1))



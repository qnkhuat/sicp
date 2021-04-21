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

(define (prime? n)
  (define (prime-iter i)
    (cond ((= n i) #t)
          ((= (remainder n i) 0) #f)
          (else (prime-iter (+ i 1)))))

  (if (< n 2) 
    #f
    (prime-iter 2)))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))


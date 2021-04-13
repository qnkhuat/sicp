;def fib(n):
;   n1 = 1
;   n2 = 1
;   for i in range(2, n):
;       temp = n1
;       n1 = n2
;       n2 = sum([n1, temp])
;   return n2


(define (fib-iter i n1 n2 n)
        (if (= i n)
            n2
            (fib-iter (+ i 1) n2 (+ n1 n2) n)))

(define (fib n)
    (fib-iter 1 1 1 n)
)
        


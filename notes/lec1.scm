; Run scheme by M-x run-scheme
; Oh this is a mistake I think. You thik?
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
    (cond ((or (= k 0) (= k n)) 1)
	  ((= (remainder k 2) 0) 2)
	  ((= (remainder k 2) 1) 4)
	  ))
  (define get-h (/ (- b a) n))

  (define (compute k)
    (* (/ get-h 3) (get-coff k) (f (+ a (* k get-h)))
       ))
  
  (define (iter k)
    (if (> k n)
	0
	(+ (compute k) (iter (+ k 1))
	)
    ))
    
  (iter 0)
  )
; C-@ -> M-w -> C-y  
(simpson cube 0 1 1000)

; 130

; Linear recursion
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

; Iteratively
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ result (term a)))
  (iter a 0))))

; 1.31
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b)))
  )

(define (product term a next b)
  (define (iter a result)
   (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 0))

(define (id x) x)
(define product id 1 (lambda (x) (+ x 1)) 4)







    
; 1.35
(define tolerance 0.00001)

(define (average x y) (/ (+ x y)2 ))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt x) (fixed-point (lambda (y) (average y (/x y))) 1.0 ))

(sqrt 5)


(define phi (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1))
phi

;1.40
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))





(define abs (lambda (x) (if (> x 0) x (- x))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.0001))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
	midpoint
	(let ((test-value (f midpoint)))
	  (cond ((postive? test-value)
		 (search f neg-point midpoint))
		((negative? test-value)
		 (search f midpoint pos-point))
		(else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
	(b-value (f b)))
    (cond ((and (negative? a-value) (postive? b-value))
	   (search f a b))
	  ((and (negative? b-value) (postive? a-value))
	   (search f b a))
	  (else (error "Values are not of opposite sign" a b)))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))






















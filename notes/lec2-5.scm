; Systems with Generic Operations

(define f "lec2-5.scm")
(define l load)

(l "util.scm")

; 2.5.1 Generic Arithmetic Operations
(define (install-scheme-number-package)
  (define (tag op)
    (attach-tag 'scheme-number op))

  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))

  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))

  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))

  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))

  (put 'make 'scheme-number
       (lambda (x) (tag x)))

  (put 'equ? '(scheme-number scheme-number) 
       (lambda (x y) (= x y))) 
  )


; Rational numbers
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
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
  (define (equ? x y) 
    (and (= (real-part x) (real-part y)) (= (imag-part x) (imag-part y)))) 

  ;; interface to rest of the system
  (put 'equ? '(rational rational) equ?) 
  (define (tag op) (attach-tag 'rational op))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

; Complex numbers
; has two levels
; the top is Complex itself
; under it is rectangle and polar packages

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))

  (define (make-from-real-imag x y) (cons x y))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)


(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))

  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  ;; interface to rest of the system
  (define (tag op) (attach-tag 'complex op))

  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)

  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))

  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))

  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))

  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))

  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))

  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)


(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))


; Ex 2.78
(define (attach-tag type-tag contents) 
  (if (number? contents) 
    contents 
    (cons type-tag contents))) 

(define (type-tag datum) 
  (cond ((number? datum) 'scheme-number) 
        ((pair? datum) (car datum)) 
        (else (error "Wrong datum -- TYPE-TAG" datum)))) 

(define (contents datum) 
  (cond ((number? datum) datum) 
        ((pair? datum) (cdr datum)) 
        (else (error "Wrong datum -- CONTENTS" datum)))) 

; Ex 2.79   



; 2.5.2: Combining Data of Different types
; Problem: How do we make operations between different data types?

; Method 1 : Coercion
; The idea is ot add methods that transform data from one type to another
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (let ((content-args (map contents args)))
          (fold-left (lambda (x y) (apply proc (list x y))) (car content-args) (cdr content-args)))
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            ((let ((t1->t2 (get-coercion type1 type2))
                   (t2->t1 (get-coercion type2 type1)))
               (cond (t1->t2
                       (apply-generic op (t1->t2 a1) a2))
                     (t2->t1
                       (apply-generic op a1 (t2->t1 a2)))
                     (else
                       (error "No method for these types(2)"
                              (list op type-tags)))))))
          (fold-left (lambda (x y) (apply-generic op x y)) (car args) (cdr args)))))))

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))
(put-coercion 'scheme-number 'complex scheme-number->complex)

; Hierarchies of types
;Ex 2.81
(install-scheme-number-package) 
(install-rational-package)
(install-complex-package) 

(define (add x y z) (apply-generic 'add x y z))
(define (make-rational n d)
  ((get 'make 'rational) n d))
(define r1 (make-rational 3 4))
(define r2 (make-rational 1 4))
(define r3 (make-rational 5 4))
(add r1 r2 r3)
(add 1 2 3)



















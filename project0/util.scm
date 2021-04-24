; Find square root using Newton's method

(define (abs x) (if (< x 0) (- x) x))
(define tolerance 0.01001)
(define (good-enough? new-guess last-guess)
  (if (< (abs (- new-guess last-guess)) tolerance)
    #t
    #f))

(define (square-root n)
  (define (square-root-guess guess)
    (let ((new-guess (/ (+ (/ n guess) guess) 2)))
      (if (good-enough? new-guess guess)
        new-guess
        (square-root-guess new-guess))
      )) 
  (cond ((< n 0) nil)
        ((= n 0) 0)
        (else (square-root-guess n))))


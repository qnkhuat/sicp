; Find square root using Newton's method

(define nil '())
(define (abs x) (if (< x 0) (- x) x))

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

(define (sequence low high inc)
  (if (> low  high)
    nil
    (cons low (sequence (+ low inc) high inc))))


(define (display-expect result expect )
    (let ((result (if (null? result) "null" result))
          (expect (if (null? expect) "null" expect)))
    (display "Result: ")
    (display result)
    (display "\tExpect: ")
    (display expect)
    (newline)
    ))

(define (ndisplay x)
  (newline)
  (display x)
  )


(define (displayn x)
  (display x)
  (newline))


; Systems with Generic Operations

(define f "lec3-1.scm")
(define l load)

(l "util.scm")

(define balance 100) ; balance as global state

(define (withdraw amount)
  (if (>= balance amount)
    (begin (set! balance (- balance amount))
           balance)
    "Insufficient funds"))


;; Turn the balance into a local state
(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))))


(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds")))


; Message passing method
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds")))


(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))


(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
        (cond ((= trials-remaining 0)
               (/ trials-passed trials))
              ((= (gcd x1 x2) 1)
               (iter (- trials-remaining 1)
                     (+ trials-passed 1)
                     x2))
              (else
               (iter (- trials-remaining 1)
                     trials-passed
                     x2))))))
  (iter trials 0 initial-x))

(define (distance p)
  (sqrt (+ (square (car p)) (square (cdr p)))))

(distance (cons 3 4))
(define (estimate-pi)
  (if (< (distance (cons (random 1.0) (random 1.0))) 1)
    #t
    #f))


(define pi (* 4 (monte-carlo 1000 estimate-pi)))



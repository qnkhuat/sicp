; Concurrency: Time is the Essence
(define f "lec3-3.scm")
(define l load)

(l "util.scm")


; Implement serializers
; Based on the concept of mutext
; A mutex is an object that supports two operations – the mutex can be acquired, and the mutex can be released.
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))


(define (make-mutex)
  (let ((cell (list #f)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
               (the-mutex 'acquire)); retry
             ((eq? m 'release) (clear! cell)))))
    the-mutex))

(define (clear! cell)
  (set-car! cell #f))


(define (test-and-set! cell)
  (if (car cell)
    #t
    (begin
      (set-car! cell #t)
      #f)))

; Ex 3.48
(define (serialized-exchange account1 account2) 
  (let ((serializer1 'serializer-for-bigger-id-acc) 
        (serializer2 'serializer-for-smaller-id-acc)) 
    (cond ((> (get-id account1) (get-id account2)) 
           (set! serializer1 (account1 'serializer)) 
           (set! serializer2 (account2 'serializer))) 
          (else (set! serializer1 (account2 'serializer)) 
                (set! serializer2 (account1 'serializer)))) 
    ((serializer1 (serializer2 exchange)) account1 account2))) 































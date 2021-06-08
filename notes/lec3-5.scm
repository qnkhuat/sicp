; Stream
(define f "lec3-5.scm")
(define l load)

(l "util.scm")


(define (map-stream proc s)
  (if (empty-stream? s)
    the-empty-stream
    (cons-stream
      (proc (head s))
      (map-stream proc (tail s)))))



(define (flatten stream-of-streams)
  (fold-left
    append
    '() 
    stream-of-streams
    )
  )

(flatten (list (list 1 2 3) (list 4 5 6)))

(define flatmap (f s)
  (flatten (map f s)))


(define (prime-sum-pairs n)
  (collect
    (list i j (+ i j))
    ((i (enum-internval 1 n))
     (j (enum-interval 1 (-1+ i))))
    (prime? (+ i j))))


(define (queens size)
  (define (fill-cols k)
    (if 
      (= k 0)
      (singleton empty-board)
      (collect 
        (adjoin-position try-row
                         k
                         rest-queens)
        ((rest-queens (fill-cols (-1+ k)))
         (try-row (enum-interval 1 size)))
        (safe? try-row k rest-queens))
      ))
  (fill-cols size))
; Another way to solve this is back-tracking => This algo is more efficient but it's harder to understand because we involved time in the process


; Ex 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map 
               (cons proc (map stream-cdr argstreams))))))

; Ex 3.51

(define (show x)
  (display-line x)
  x)


(define x (stream-map show (stream-enumerate-interval 0 10)))
; Because the stream has cache
(stream-ref x 5) ; => 1 2 3 4 5
(stream-ref x 7) ; => 6 7

; Ex 3.52
(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
    sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))

(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) 
                         seq))

(stream-ref y 7) ; =>  14th of sum accum
(display-stream z) ; => 






































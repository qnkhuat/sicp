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






























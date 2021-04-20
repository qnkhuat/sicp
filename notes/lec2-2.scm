; Buildling abstractions with Data
(define f "lec2-2.scm")
(define l load)

(l "util.scm")

;(define (append l1 l2)
;  (if (null? l1)
;    l2
;    (cons (car l1) (append (cdr l1) l2))))
;
;(define l1 (list 1 2 3))
;(define l2 (list 4 5 6))
;(append l1 l2)
;
;; E.x 2.17:
;(define (last-pair l)
;  (if (= (length l) 1)
;    (car l)
;    (last-pair (cdr l))))
;
;(last-pair (list 1 2 3))
;
;
;; Map over lists
;(map abs (list -10 3 4 5))
;(define (map proc items)
;  (if (null? items)
;    nil
;    (cons (proc (car items))
;          (map proc (cdr items))))) ; map is buitltin
;
;
;; E.x 2.23 Define for-each
;(define (for-each proc lst)
;  (if (null? lst)
;    nil
;    (and (proc (car lst))
;         (for-each proc (cdr lst))
;         )))
;
;;(for-each (lambda (x) (newline) (display x))
;;          (list 57 321 88))
;
;; 2.2.2 Hierachical Structures
;(define (count-leaves x)
;  (cond ((null? x) 0)
;        ((not (pair? x)) 1)
;        (else (+ (count-leaves (car x))
;                 (count-leaves (cdr x)))
;              )))
;
;
;
;
;
;; E.x 2.28
;(define x (list (list 1 2) (list 3 4)))
;(define x (list 1 2))
;
;(define (fringe tree) 
;  (cond ((null? tree) nil)
;        ((not (pair? tree)) (list tree))
;        (else (append (fringe (car tree)) (fringe (cdr tree))))))
;  
;(fringe x)
;
;
;; Mapping over trees
;(define (scale-tree tree factor)
;  (cond ((null? tree) nil)
;        ((not (pair? tree)) * (* tree factor))
;        (else (cons (scale-tree (car tree) factor)
;                    (ccale-tree (cdr tree) factor)))
;        ))
;
;(define (scale-tree tree factor)
;  (map (lambda (sub-tree)
;         (if (pair? sub-tree)
;           (scale-tree sub-tree factor)
;           (* sub-tree factor)))
;       tree))
;
;; E.x: 2.30
;(define (square-tree tree)
;  (map (lambda (sub-tree) 
;         (if (pair? sub-tree)
;           (square-tree sub-tree)
;           (expt 2 sub-tree)))
;       tree))
;
;(define tree (list 1
;       (list 2 (list 3 4) 5)
;       (list 6 7)))
;(square-tree tree)
;       
;
;; E.x: 2.31
;(define (tree-map f tree)
;  (map (lambda (sub-tree)
;         (if (pair? sub-tree)
;         (tree-map f sub-tree)
;         (f sub-tree)))
;       tree))
;
;(define square (lambda (x) (* x x)))
;(tree-map square tree)
;
;; 2.2.3 Sequences as Conventional Interfaces
;(define (sum-odd-squares tree)
;  (cond ((null? tree) 0)
;        ((not (pair? tree))
;         (if (odd? tree) (square tree) 0))
;        (else (+ (sum-odd-squares (car tree))
;                 (sum-odd-squares (cdr tree))))))
;
;
;(define (even-fibs n)
;  (define (next k)
;    (if (> k n)
;      nil
;      (let ((f (fib k)))
;        (if (even? f)
;          (cons f (next (+ k 1)))
;          (next (+ k 1))))))
;  (next 0))
;
; The idea is to combine these 2 seems to different flows int one
; If we look further we will see the 2 programs has a signal sequence as follow:
; Enumerate -> filter -> map -> accumulate

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (sumb-odd-squares tree)
  (accumulate +
              nil
              (map square (filter odd? (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate cons
              nil
              (filter even? (map fib (enumerate-interval 0 n)))))

; E.x: 2.33
(define (map p sequence) 
  (accumulate (lambda (x y) (cons (p x) y)) 
              nil 
              sequence))

(map square (list 1 2 3))

(define (append seq1 seq2)
   (accumulate cons seq1 seq2))

(append (list 1 2 3) (list 4 5 6))

;(define (length sequence)
;  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
;
;(length (list 3 2 9))




























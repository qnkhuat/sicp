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

;(define (accumulate op initial sequence)
;  (if (null? sequence)
;      initial
;      (op (car sequence)
;          (accumulate op initial (cdr sequence)))))
;
;(define (enumerate-tree tree)
;  (cond ((null? tree) nil)
;        ((not (pair? tree)) (list tree))
;        (else (append (enumerate-tree (car tree))
;                      (enumerate-tree (cdr tree))))))
;
;(define (enumerate-interval low high)
;  (if (> low high)
;      nil
;      (cons low (enumerate-interval (+ low 1) high))))
;
;(define (sumb-odd-squares tree)
;  (accumulate +
;              nil
;              (map square (filter odd? (enumerate-tree tree)))))
;
;(define (even-fibs n)
;  (accumulate cons
;              nil
;              (filter even? (map fib (enumerate-interval 0 n)))))
;
;; E.x: 2.33
;(define (map p sequence) 
;  (accumulate (lambda (x y) (cons (p x) y)) 
;              nil 
;              sequence))
;
;(map square (list 1 2 3))
;
;(define (append seq1 seq2)
;   (accumulate cons seq1 seq2))
;
;(append (list 1 2 3) (list 4 5 6))

;(define (length sequence)
;  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
;
;(length (list 3 2 9))


; E.x: 2.38
;(fold-right / 1 (list 1 2 3)); -> 3 / 2
;(fold-left  / 1 (list 1 2 3)); -> 1 / 6
;(fold-right cons nil (list 1 2 3)) ; -> list (1 2 3)
;(fold-left cons nil (list 1 2 3)) ; -> list (3 2 1)

; E.x 2.39
;(define (reverse seq)
;  (fold-left (lambda (x y) (append x (list y))) nil seq))
;
;(define (reverse seq)
;  (fold-right (lambda (x y) (append (list x) y)) nil seq))
;
;
;(reverse (list 1 2 3))


; Nested mapping

(define (flatmap proc seq)
  (fold-right append nil (map proc seq)))
;
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
;
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
;
;(define (prime-sum-pairs n)
;  (map make-pair-sum
;       (filter prime-sum?
;               (flatmap
;                (lambda (i)
;                  (map (lambda (j) (list i j))
;                       (enumerate-interval 1 (- i 1))))
;                (enumerate-interval 1 n)))))
;
;(prime-sum-pairs 10)



; E.x: 2.40
(define (unique-pairs n)
  (fold-right append nil 
              (map (lambda (i) 
                     (map (lambda (j) (list i j)) 
                          (enumerate-interval 1 (- i 1)))) 
                   (enumerate-interval 1 n))))

(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs 5)


;E.x 2.42

(define (make-pos r c) (list r c))
(define (get-row p) (car p))
(define (get-col p) (cadr p))

; rest-of-queens is a list of positions to place the last 
;
(define empty-board nil)
;
(define (adjoin-position row col rest-of-queens) 
  (cons (make-pos row col) rest-of-queens))

;(queen-row (if (= (length queen-rows) 1 ) (list-ref queen-rows 0) nil))

(define (get-pos-by-column col positions)
  (let ((found-pos (filter (lambda (pos) (= (get-col pos) col)) 
                     positions)))
    (if (= (length found-pos) 0) 
      nil
      (car found-pos))
  ))

(define (display-but-return d)
  (display "------------")
  (newline)
  (display d)
  (newline)
  d)

(define (same-row target this)
  (= (get-row target) (get-row this)))

(define (same-col target this)
  (= (get-col target) (get-col this)))

(define (same-diagonal target this)
  (let ((col-diff (- (get-col this) (get-col target))))
    (or (= (get-row this) (+ col-diff (get-row target)))
         (= (get-row this) (- (get-row target) col-diff)))))

(define (get-queen-at-col col positions) (car(filter (lambda (pos) (= (get-col pos) col)) positions)))
(define (get-queens-not-at-col col positions) (filter (lambda (pos) (not (= (get-col pos) col)
                                                                         )) positions))

(define (safe? col positions)
  (let ((queen-at-col (get-queen-at-col col positions))
        (queens-not-at-col (get-queens-not-at-col col positions)))
    (null? (filter (lambda (queen) (or 
                                     (same-row queen queen-at-col) 
                                     (same-diagonal queen queen-at-col)))
    queens-not-at-col))))

 (define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define q (queens 8))

; EXAMPLE : A Picture language

(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))

(define (flipped-paris painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define wave4 (flipped-pairs wave))

(define (right-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (right-split painter (- n 1))))
      (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
    painter
    (let ((up (up-split painter (- n 1)))
          (right (right-split paitner (- n 1)))))))


























































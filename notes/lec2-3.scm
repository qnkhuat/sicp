; Buildling abstractions with Data
(define f "lec2-3.scm")
(define l load)

(l "util.scm")

; Quotation
(define a 1)
(define b 2)
(list a b) ; => (1 2)
(list 'a 'b) ; => (a b)
(list 'a b) ; => (a 2)

(car '(a b 2)) ; => a
(cdr '(a 2)) ; => a


(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x )) x)
        (else (memq item (cdr x)))))

(memq 'apple '(pear apple juice))

; Ex 2.54
(define (equal? l1 l2)
  (cond ((and (pair? l1) (pair? l2)) (and (eq? (car l1) (car l2)) (equal? (cdr l1) (cdr l2))))
        ((and (not (pair? l1)) (not (pair? l2))) (eq? l1 l2))
        (else #f)))

(equal? 2 2) ;#t
(equal? 2 3) ;#f
(equal? '(2) '(2)); #t
(equal? '(this is a list) '(this is a list)) ;#t
(equal? '(this is a list) '(this (is a) list)) ;#f
    

; 2.3.2 Example: Symbolic Differentiation
; (deriv '(* x y) 'x) => y
; (deriv '(+ x 3) 'x) => 1

(define (variable? e) 
  (symbol? e))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
;Ex 2.56 
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base s) (cadr s))
(define (exponent s) (caddr s))

(define (make-exponentiation b e) 
        (list '** b (- e 1)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (multiplier s) (cadr s))

(define (multiplicand s) (caddr s))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((exponentiation? exp) 
         (make-product 
           (make-product 
             (exponent exp) (make-exponentiation (base exp) (exponent exp)))
           (deriv (base exp) var)
           ))
        ((product? exp) 
         (make-sum 
           (make-product 
             (multiplier exp) 
             (deriv (multiplicand exp) var)))
         (make-product 
           (multiplicand exp) 
           (deriv (multiplier exp) var)))
        ((sum? exp) 
         (make-sum 
           (deriv (addend exp) var) 
           (deriv (augend exp) var)))
        (else (error "Unknown expression type " exp))))


(deriv '(+ x 3) 'x)
(deriv '(** x 3) 'x)


; 2.3.3 Example: Representing Sets
; Unordered list as set
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))


; Ex 2.59
(define (union-set s1 s2)
  (if (null? s1) 
    s2 
    (union-set
      (cdr s1)
      (if (element-of-set? (car s1) s2) 
        s2 
        (cons (car s1) s2))
      )))


; Ordered list as set
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))


(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((= x1 x2)
             (cons x1
                   (intersection-set (cdr set1)
                                     (cdr set2))))
            ((< x1 x2)
             (intersection-set (cdr set1) set2))
            ((< x2 x1)
             (intersection-set set1 (cdr set2)))))))

; 2.62
; Union set is the set that contains either element of set1 or set2
(define (union-set s1 s2)
  (if (null? s1)
    s2
    (let ((x1 (car s1)) (x2 (car s2)))
      (cond ((= x1 x2) 
             (cons x1 (union-set s1 s2)))
            ((< x1 x2) 
             (car x1 (union-set (cdr s1) s2)))
            ((< x2 x1) 
             (car x2 (union-set (cdr s2) s1))))
      )
    )
  )

  
; 2.3.4 Example: Huffman Encoding Trees
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x)) 
(define (weight-leaf x) (caddr x)) 

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbosl right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))









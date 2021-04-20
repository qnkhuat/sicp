# Lec 1
Computer Science is not about Science or Computer
The Computer is just the tool we use, 
Computer Science is about Human trying to learn to build process and even big systems

## Technique to control complexity
> The constraints of building large software systems are the limitations of our own mind. Contrary to what engineer build, they are limited in the physical limitation of the sytstem.
1. Black Box Abstraction
2. Establishing Conventional Interface
- Generic operations
- Large-scale structure and modularity
- Object-oriented Programming
- Operations on aggregates
3. Metaliguistc Abstraction 
- Make a new language that fit the complexity of system 


## General framework to think about programming language
- What are the primitive elements?
- What are the means of combination? => How to combine primitive elements
- What are the means of abstraction? => How to build black-box

Combination in lisp

```lisp
(+ 3 17.4 5)
(+ 2 (* 2 3) 8 9)
(+ 2 (* 2 3) 
     8 
     9)
```
This called prefix notation -> operator is to the left of operands

Abstraction in lisp

```lisp
(define (square x) (* x x))

(define (sum_square x) (+ (square x) (square x)))

(define square (lambda (x) (* x x)))
```

Syntactic sugar : when 2 codes express the same function (I.e the square define by lambda vs normal form above). It's syntactic sugar. 
Because for interpreter they are the same.

Case analysis

```lisp
(define (abs x)
    (cond ((< x 0) (- x))
          ((= x 0) 0)
          ((> x 0) x)
    )
)
```


# Lec 2 - Procedures and Processes; Substitution Model
```lisp
(define (sos x y)
    (+ (sq x) (sq y)))

(define (sq x )
    ( * x x ))
```

Kind of expressions:
- Numbers
- Symbols
- Combination
- Lambda expressoins
- Definition














































































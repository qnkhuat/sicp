; Logic programming
; In Logic programming we combines a relational vision of programming with a powerful kind of symbolic pattern matching called unification
(define f "lec4-1.scm")
(define l load)

(l "util.scm")

;(job ?x (computer ?type)) ; => Matches all jobs entries who third item is a two-elemtn lsit whose first item i computer
;
;(job ?x (computer . ?type)) ; => Match all jobs entries whor thid item has a computer in it
;
;(supervisor ?x ?x) find all people who supervise themselves

; Ex 4.55
;a (supervisor ?person (Ben Bidddle)) ; => all people supervised by Ben Bitdiddle;

;b (job ?person (accouning . ?work)) ; => the names and jobs of all people in the accounting division

;c (address ?person (Summervail . ?address) ; => the names and addresses of all people who live in Slumervill
















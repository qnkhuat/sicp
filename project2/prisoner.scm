;; 
;;  The play-loop procedure takes as its  arguments two prisoner's
;;  dilemma strategies, and plays an iterated game of approximately
;;  one hundred rounds.  A strategy is a procedure that takes
;;  two arguments: a history of the player's previous plays and 
;;  a history of the other player's previous plays.  The procedure
;;  returns either a "c" for cooperate or a "d" for defect.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define l load)
(define f "prisoner.scm")
(l "util.scm")

(define (play-loop strat0 strat1)
  (define (play-loop-iter strat0 strat1 count history0 history1 limit)
    (cond ((= count limit) (print-out-results history0 history1 limit))
	  (else (let ((result0 (strat0 history0 history1))
		      (result1 (strat1 history1 history0)))
		  (play-loop-iter strat0 strat1 (+ count 1)
				  (extend-history result0 history0)
				  (extend-history result1 history1)
				  limit)))))
  (play-loop-iter strat0 strat1 0 the-empty-history the-empty-history
		  (+ 90 (random 21))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The following procedures are used to compute and print
;;  out the players' scores at the end of an iterated game
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-out-results history0 history1 number-of-games)
  (let ((scores (get-scores history0 history1)))
    (newline)
    (display "Player 1 Score:  ")
    (display (* 1.0 (/ (car scores) number-of-games)))
    (newline)
    (display "Player 2 Score:  ")
    (display (* 1.0 (/ (cadr scores) number-of-games)))
    (newline)))

(define (get-scores history0 history1)
  (define (get-scores-helper history0 history1 score0 score1)
    (cond ((empty-history? history0)
	   (list score0 score1))
	  (else (let ((game (make-play (most-recent-play history0)
				       (most-recent-play history1))))
		  (get-scores-helper (rest-of-plays history0)
				     (rest-of-plays history1)
				     (+ (get-player-points 0 game) score0)
				     (+ (get-player-points 1 game) score1))))))
  (get-scores-helper history0 history1 0 0))

(define (get-player-points num game)
  (list-ref (get-point-list game) num))

(define *game-association-list*
  ;; format is that first sublist identifies the players' choices 
  ;; with "c" for cooperate and "d" for defect; and that second sublist 
  ;; specifies payout for each player
  '((("c" "c") (3 3))
    (("c" "d") (0 5))
    (("d" "c") (5 0))
    (("d" "d") (1 1))))

(define make-play list)

(define (get-point-list game)
  (cadr (extract-entry game *game-association-list*)))

(define (extract-entry game *game-association-list*)
  (define (extract-iter game part-list)
    (let ((current (car part-list)))
      (if (equal? game (car current)) current
          (extract-iter game (cdr part-list)))))
  (extract-iter game *game-association-list*))

;;; test extract-entry
(define a-play (make-play "c" "d"))
(display-expect (extract-entry a-play *game-association-list*) (list (list "c" "d") (list 0 5)))

;; note that you will need to write extract-entry


(define the-empty-history '())

(define extend-history cons)
(define empty-history? null?)

(define most-recent-play car)
(define rest-of-plays cdr)

;; A sampler of strategies

(define (NASTY my-history other-history)
  "d")

(define (PATSY my-history other-history)
  "c")

(define (SPASTIC my-history other-history)
  (if (= (random 2) 0)
      "c"
      "d"))

(define (EGALITARIAN my-history other-history)
  (define (count-instances-of test hist)
    (cond ((empty-history? hist) 0)
	  ((string=? (most-recent-play hist) test)
	   (+ (count-instances-of test (rest-of-plays hist)) 1))
	  (else (count-instances-of test (rest-of-plays hist)))))
  (let ((ds (count-instances-of "d" other-history))
	(cs (count-instances-of "c" other-history)))
    (if (> ds cs) "d" "c")))

(define (EYE-FOR-EYE my-history other-history)
  (if (empty-history? my-history)
      "c"
      (most-recent-play other-history)))

; Problem 4
(define (EYE-FOR-TWO-EYES my-history other-history)
  (cond ((< (length other-history) 2) "c")
         ((and 
           (string=? (list-ref other-history (- (length other-history) 1)) "d")
           (string=? (list-ref other-history (- (length other-history) 2)) "d") "d"))
         (else "c")))

; Problem 5
(define (make-eye-for-n-eyes n)
  (define (is-n-d history n)
    (if (< n 1)
      #t
      (and (is-n-d history (- n 1)) 
                   (string=? (list-ref history (- (length history) n )) "d"))))
  (define (func history other-history)
    (cond ((< (length other-history) n) "c")
          ((is-n-d other-history n) "d")
          (else "c")))
  func)




(define (play-each-others players-with-name)
  (define (iter player1 rest-players)
    (if (null? rest-players)
      nil 
      (let ((player2 (car rest-players)))
        (display (string-append (car player1) " vs " (car player2)))
        (play-loop (cdr player1) (cdr player2))
        (newline)
        (iter player1 (cdr rest-players))
        )
      ))
  (if (null? players-with-name)
    nil 
    (begin
      (iter (car players-with-name) players-with-name)
      (play-each-others (cdr players-with-name))
      )))



; Problem 6
(define (make-rotating-strategy strat0 strat1 freq0 freq1)
  (define (func history other-history)
    (let ((game-number (- (length history) 1)))
      (if (< (remainder game-number (+ freq0 freq1 )) freq0)
        (strat0 history other-history)
        (strat1 history other-history))))
  func)

; Problem 8
(define (gentle strat gentleness-factor)
  (lambda (my-history other-history)
    (let ((result (strat my-history other-history)))
      (if (string=? result "c") "c"
          (let ((random-factor (/ (random 100) 100)))
            (if (< random-factor gentleness-factor)
                "c"
                "d"))))))

(define SLIGHTLY-GENTLE-NASTY (gentle NASTY 0.1))
(define SLIGHTLY-GENTLE-EYE-FOR-EYE (gentle EYE-FOR-EYE 0.1))

(define players-with-name
  (list 
    (cons "EYE-FOR-TWO-EYES" EYE-FOR-TWO-EYES)
    (cons "EYE-FOR-N-EYES" (make-eye-for-n-eyes 6))
    (cons "MULTI-FACETS" (make-rotating-strategy NASTY PATSY 10 10))
    (cons "NASTY" NASTY)
    (cons "PATSY" PATSY)
    (cons "EGALITARIAN" EGALITARIAN)
    (cons "SPASTIC" SPASTIC)
    (cons "EYE-FOR-EYE" EYE-FOR-EYE)
    ))
;(play-each-others players-with-name)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; code to use in 3 player game
;;	    

(define *game-association-list*
  (list (list (list "c" "c" "c") (list 4 4 4))
        (list (list "c" "c" "d") (list 2 2 5))
        (list (list "c" "d" "c") (list 2 5 2))
        (list (list "d" "c" "c") (list 5 2 2))
        (list (list "c" "d" "d") (list 0 3 3))
        (list (list "d" "c" "d") (list 3 0 3))
        (list (list "d" "d" "c") (list 3 3 0))
        (list (list "d" "d" "d") (list 1 1 1))))


;; in expected-values: #f = don't care 
;;                      X = actual-value needs to be #f or X 
;(define (test-entry expected-values actual-values) 
;   (cond ((null? expected-values) (null? actual-values)) 
;         ((null? actual-values) #f) 
;         ((or (not (car expected-values)) 
;              (not (car actual-values)) 
;              (= (car expected-values) (car actual-values))) 
;          (test-entry (cdr expected-values) (cdr actual-values))) 
;         (else #f))) 
;
;(define (is-he-a-fool? hist0 hist1 hist2) 
;   (test-entry (list 1 1 1) 
;               (get-probability-of-c 
;                (make-history-summary hist0 hist1 hist2))))
;
;(define (could-he-be-a-fool? hist0 hist1 hist2)
;  (test-entry (list 1 1 1)
;              (map (lambda (elt) 
;                      (cond ((null? elt) 1)
;                            ((= elt 1) 1)  
;                            (else 0)))
;                   (get-probability-of-c (make-history-summary hist0 
;                                                               hist1
;                                                               hist2)))))


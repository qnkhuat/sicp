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
(define f "prisoner2.scm")
(l "util.scm")

(define (play-loop strat0 strat1 strat2)
  (define (play-loop-iter strat0 strat1 strat2 count history0 history1 history2 limit)
    (cond ((= count limit) (print-out-results history0 history1 history2 limit))
	  (else (let ((result0 (strat0 history0 history1 history2))
		      (result1 (strat1 history1 history0 history2))
		      (result2 (strat2 history1 history0 history2))
                      )
		  (play-loop-iter strat0 strat1 strat2 (+ count 1)
				  (extend-history result0 history0)
				  (extend-history result1 history1)
				  (extend-history result2 history2)
				  limit)))))
  (play-loop-iter strat0 strat1 strat2 0 the-empty-history the-empty-history the-empty-history
		  (+ 90 (random 21))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The following procedures are used to compute and print
;;  out the players' scores at the end of an iterated game
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-out-results history0 history1 history2 number-of-games)
  (let ((scores (get-scores history0 history1 history2)))
    (newline)
    (display "Player 1 Score:  ")
    (display (* 1.0 (/ (car scores) number-of-games)))
    (newline)
    (display "Player 2 Score:  ")
    (display (* 1.0 (/ (cadr scores) number-of-games)))
    (newline)
    (display "Player 3 Score:  ")
    (display (* 1.0 (/ (caddr scores) number-of-games)))
    (newline)))

(define (get-scores history0 history1 history2)
  (define (get-scores-helper history0 history1 history2 score0 score1 score2)
    (cond ((empty-history? history0)
	   (list score0 score1 score2))
	  (else (let ((game (make-play (most-recent-play history0)
				       (most-recent-play history1)
				       (most-recent-play history2))))
		  (get-scores-helper (rest-of-plays history0)
				     (rest-of-plays history1)
				     (rest-of-plays history2)
				     (+ (get-player-points 0 game) score0)
				     (+ (get-player-points 1 game) score1)
				     (+ (get-player-points 2 game) score2))))))
  (get-scores-helper history0 history1 history2 0 0 0))


(define (get-player-points num game)
  (list-ref (get-point-list game) num))

(define *game-association-list*
  (list (list (list "c" "c" "c") (list 4 4 4))
        (list (list "c" "c" "d") (list 2 2 5))
        (list (list "c" "d" "c") (list 2 5 2))
        (list (list "d" "c" "c") (list 5 2 2))
        (list (list "c" "d" "d") (list 0 3 3))
        (list (list "d" "c" "d") (list 3 0 3))
        (list (list "d" "d" "c") (list 3 3 0))
        (list (list "d" "d" "d") (list 1 1 1))))

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
(define a-play (make-play "c" "d" "c"))
(display-expect (extract-entry a-play *game-association-list*) (list (list "c" "d" "c") (list 2 5 2)))

;; note that you will need to write extract-entry

(define the-empty-history '())

(define extend-history cons)
(define empty-history? null?)

(define most-recent-play car)
(define rest-of-plays cdr)

;; A sampler of strategies

(define (NASTY my-history history1 history2)
  "d")

(define (PATSY my-history hsitory1 history2)
  "c")

(define (SPASTIC my-history history1 history2)
  (if (= (random 2) 0)
      "c"
      "d"))

(define (EGALITARIAN my-history history1 history2)
  (define (count-instances-of test hist)
    (cond ((empty-history? hist) 0)
	  ((string=? (most-recent-play hist) test)
	   (+ (count-instances-of test (rest-of-plays hist)) 1))
	  (else (count-instances-of test (rest-of-plays hist)))))
  (let ((ds (count-instances-of "d" history1))
	(cs (count-instances-of "c" history1)))
    (if (> ds cs) "d" "c")))


(define (TOUGH-EYE-FOR-EYE my-history player1-history player2-history)
  (if (empty-history? my-history)
      "c"
      (let ((previous1 (most-recent-play player1-history))
            (previous2 (most-recent-play player2-history)))
        (if (or (string=? previous1 "d") (string=? previous2 "d"))
            "d"
            "c"))))

(define (SOFT-EYE-FOR-EYE my-history player1-history player2-history)
  (if (empty-history? my-history)
      "c"
      (let ((previous1 (most-recent-play player1-history))
            (previous2 (most-recent-play player2-history)))
        (if (and (string=? previous1 "d") (string=? previous2 "d"))
            "d"
            "c"))))

(define (play-each-others players-with-name)
  (define (iter1 player1 rest-players)
    (if (null? rest-players)
      nil 
      (let ((player2 (car rest-players)))
        (iter2 player1 player2 (cdr rest-players)))))

  (define (iter2 player1 player2 rest-players)
    (if (null? rest-players)
      nil 
      (let ((player3 (car rest-players)))
        (display (string-append (car player1) " vs " (car player2) " vs " (car player3)))
        (play-loop (cdr player1) (cdr player2) (cdr player3))
        (newline)
        (iter2 player1 player2 (cdr rest-players)))))

  (if (null? players-with-name)
    nil 
    (begin
      (iter1 (car players-with-name) players-with-name)
      (play-each-others (cdr players-with-name))
      )))


(define players-with-name
  (list 
    (cons "NASTY" NASTY)
    (cons "PATSY" PATSY)
    (cons "EGALITARIAN" EGALITARIAN)
    (cons "SPASTIC" SPASTIC)
    (cons "TOUGH-EYE-FOR-EYE" TOUGH-EYE-FOR-EYE)
    (cons "SOFT-EYE-FOR-EYE" SOFT-EYE-FOR-EYE)))

(play-each-others players-with-name)

(list 
  (list (list "c" "c") (list 0 1 2))
  (list (list "c" "d") (list 0 1 2))
  (list (list "d" "d") (list 0 1 2))
  )

; Problem 12
(define (add-case case play)
  (cond ((equal? play "c")
         (list (+ (list-ref case 0) 1)
               (list-ref case 1)
               (+ (list-ref case 2) 1)))

        ((equal? play "d")
         (list (list-ref case 0  )
               (+ (list-ref case 1) 1)
               (+ (list-ref case 2) 1)))
        (else case)))

(define (make-history-summary history0 history1 history2)
  (define intitial-case (list 0 0 0))
  (define (make-iter zipped-history cc cd dd)
    (if (< (length zipped-history) 2)
      (list cc cd dd)
      (let ((next-case (cdr (list-ref zipped-history 1)))
            (current-play (car (list-ref zipped-history 0))))
        (cond ((equal? next-case (list "c" "c")) 
               (make-iter (cdr zipped-history) (add-case cc current-play) cd dd))
              ((or (equal? next-case (list "c" "d")) (equal? next-case (list "d" "c")))
               (make-iter (cdr zipped-history) cc (add-case cd current-play) dd))
              ((equal? next-case (list "d" "d")) 
               (make-iter (cdr zipped-history) cc cd (add-case dd current-play) ))
              (else nil)))))
  (make-iter (map list history0 history1 history2) intitial-case intitial-case intitial-case))

(make-history-summary 
  (list "c" "c" "d" "d" "c" "d" "c" "c")
  (list "c" "c" "c" "d" "d" "c" "d" "c")
  (list "c" "c" "d" "d" "d" "c" "c" "c"))


; Problem 13



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


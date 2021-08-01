; Stream
(define f "lec5-1.scm")
(define l load)

(l "util.scm")

(define (gcd a b) (if (= b 0) a (gcd b (remainder a b))))

(define (factorial n) 
  (define (iter product counter) 
    (if (> counter n) 
      product 
      (iter (* counter product) (+ counter 1)))) 
  (iter 1 1))

;(data-paths
;   (registers
;        ((name a)
;            (buttons ((name a<-b) (source (register b)))))
;        ((name b)
;               (buttons ((name b<-t) (source (register t)))))
;        ((name t)
;               (buttons ((name t<-r) (source (operation rem))))))
;
;   (operations
;        ((name rem)
;               (inputs (register a) (register b)))
;        ((name =)
;               (inputs (register b) (constant 0)))))
;
;(controller
;   test-b                           ; label
;     (test =)                       ; test
;     (branch (label gcd-done))      ; conditional branch
;     (t<-r)                         ; button push
;     (a<-b)                         ; button push
;     (b<-t)                         ; button push
;     (goto (label test-b))          ; unconditional branch
;   gcd-done)                        ; label”
;
;; GCD 
;(controller 
;  test-b 
;  (test (op =) (reg b) (const 0)) 
;  (branch (label gcd-done)) 
;  (assign t (op rem) (reg a) (reg b)) 
;  (assign a (reg b)) 
;  (assign b (reg t)) 
;  (goto (label test-b)) 
;  gcd-done)
;
;; Factorial
;(controller  
;  test-n 
;  (test (op >) (reg Count) (reg n)) 
;  (branch (label done)) 
;  (assign t (op *) (reg Prod) (reg Count)) 
;  (assign Prod (reg t)) 
;  (assign t (op +) (reg Count) (const 1)) 
;  (assign Prod (reg t)) 
;  (goto (label test-n)) 
;  done)  
;
;
;; With perform
;
;(controller 
;  gcd-loop 
;  (assign a (op read)) 
;  (assign b (op read)) 
;  test-b 
;  (test (op =) (reg b) (const 0)) 
;  (branch (label gcd-done)) 
;  (assign t (op rem) (reg a) (reg b)) 
;  (assign a (reg b)) 
;  (assign b (reg t)) 
;  (goto (label test-b)) 
;  gcd-done 
;  (perform (op print) (reg a)) 
;  (goto (label gcd-loop))
;  )



(make-machine <register-names> <operations> <controller>)
(set-register-contents! <machine-model> <register-name> <value>)
(get-register-contents <machine-model> <register-name>)
(start <machine-model>)























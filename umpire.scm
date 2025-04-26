;; umpire.scm

;; Primitive rules

(define (valid-pick? state)
  (not (null? (cdr (get-deck state)))))

(define (valid-place-in-stack? state bucket-index)
  (let ((hand (cdr (get-player-hand state))))
    (and (not (null? hand))
         (let* ((card (car hand))
                (value (card-value card))
                (current (get-bucket-value state bucket-index)))
           (<= (+ value current) 21)))))

;; Logical combinators

(define (rule-or r1 r2)
  (lambda (state)
    (or (r1 state) (r2 state))))

;; Build umpire

(define (build-umpire pick-rule place-rule)
  (display "\nThis is being called\n")
  (lambda (state move-type)
    (cond
     ((string=? move-type "pick") (pick-rule state))
     ((string=? move-type "place")
      (let loop ((i 0))
        (cond
         ((= i 4) #f) ;; no stack can take it
         ((valid-place-in-stack? state i) #t)
         (else (loop (+ i 1))))))
     (else
      (begin
        (display "Invalid move type to umpire.\n")
        #f)))))

;; Default rules: can pick if deck not empty, can place if any stack accepts
(define umpire
  (build-umpire valid-pick? valid-place-in-stack?))

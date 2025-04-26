;; umpire.scm

;; primitive rules
(define (valid-pick? state)
  (and (null? (cdr (get-player-hand state))) ; empty hand
       (not (null? (cdr (get-deck state)))))) ; non-empty deck
 
(define (valid-place-in-stack? state bucket-index)
  (and (not (null? (cdr (get-player-hand state)))) ; non-empty hand
       (and (>= bucket-index 0) (<= bucket-index 3)))) ; bucket in [0, 1, 2, 3]
  
;; build umpire
(define (build-umpire pick-rule place-rule)
  (lambda (state move-type . args)
    (cond
     ((string=? move-type "pick") (pick-rule state))
     ((string=? move-type "place") (place-rule state (car args)))
     (else
      (begin
        (display "Invalid move type to umpire.\n")
        #f)))))

;; default rules: can pick if deck not empty, can place if any stack accepts
(define umpire
  (build-umpire valid-pick? valid-place-in-stack?))

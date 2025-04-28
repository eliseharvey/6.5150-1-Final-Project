;; umpire.scm


;; (valid-pick? state) -> boolean
;; returns a truth value if the pick move (given current state) is valid. I.e. the hand is empty and the deck is non-empty
(define (valid-pick? state)
  (and (null? (cdr (get-player-hand state))) ; empty hand
       (not (null? (cdr (get-deck state)))))) ; non-empty deck


;; (valid-place-in-stack? state bucket-index) -> boolean
;; reutrns a truth value if the palce move is valid. I.e. the hand is non-empty and the bucket number exists
(define (valid-place-in-stack? state bucket-index)
  (and (not (null? (cdr (get-player-hand state)))) ; non-empty hand
       (and (>= bucket-index 1) (<= bucket-index 4)))) ; bucket in [1, 2, 3, 4]


;; (build-umpire pick-rule place-rule) -> boolean
;; wraps the rules defined by the creator and sets up when each move should be applied. Using lambda enables user to define what variable would be expected
(define (build-umpire pick-rule place-rule)
  (lambda (state move-type . args) ; expects state, move-type, and optionally bucket-number
    (cond
     ((string=? move-type "pick") (pick-rule state))
     ((string=? move-type "place") (place-rule state (car args)))
     (else
      (begin
        (display "Invalid move type to umpire.\n")
        #f)))))


;; (umpire) -> procedure
;; how game-commands calls the user defined umpire (interacting with rules)
(define umpire
  (build-umpire valid-pick? valid-place-in-stack?))

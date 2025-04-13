;; umpire.scm


(define (umpire state pick-or-place)
  (cond ((string=? pick-or-place "pick") #t) ; for now, return true since we can pick card
        ((string=? pick-or-place "place") (not (null? (cdr (assoc 'hand state))))) ; true if hand is not empty
        (else (display "Invalid use of the umpire."))))


(define (build-umpire rules)
  (umpire)) ; TODO: have this function take in rules and return an umpire that determines legality

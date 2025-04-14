;; game-commands.scm


(define (update-with-pick state)
  (let ((deck (cdr (get-deck state)))
        (hand (cdr (get-player-hand state))))
    (if (null? deck)
        (begin
          (display "Deck is empty, cannot pick a card.")
          (newline)
          state) ;; Return unchanged state
        (let ((card (car deck))          ; Get the top card
              (new-deck (cdr deck)))     ; Remove top card from deck
          (let ((new-hand (cons card hand))) ; Add card to hand
            (display "Card picked: ")
            (display card)
            (newline)
            (display "New hand: ")
            (display new-hand)
            (newline)
            ;; Create updated state
            (let ((updated-state (update-deck state new-deck)))
              (update-player-hand updated-state new-hand)))))))


(define (pick-card state)
  (if (umpire state "pick")
      (update-with-pick state)
      (display "You cannot pick a card.")))


(define (place-card state bucket-number)
  (let ((hand (cdr (get-player-hand state)))
        (buckets (get-bucket state))
        (valid-move? (umpire state "place")))
    (if valid-move?
            (let* ((card (car hand))
                   (new-hand (cdr hand))
                   (bucket-index (- bucket-number 1)))
              (if (not (and (>= bucket-index 0) (< bucket-index (length buckets))))
                  (begin
                    (display "Invalid bucket number.")
                    (newline)
                    state)
                  (let* ((card-value (card-value card))
                         (current-bucket-value (list-ref buckets bucket-index))
                         (new-bucket-value (+ current-bucket-value card-value))
                         (state-with-new-bucket (update-bucket-value state bucket-index new-bucket-value))
                         (updated-state (update-player-hand state-with-new-bucket new-hand)))
                    
                    (display "Card placed: ")
                    (display card)
                    (newline)
                    (display "New hand: ")
                    (display new-hand)
                    (newline)
                    (display "Updated buckets: ")
                    (display (get-bucket updated-state))
                    (newline)
                    
                    (if (win? updated-state)
                        (begin
                          (display "You win! Game over.")
                          (newline)
                          updated-state)
                        (if (lose? updated-state)
                            (begin
                              (display "You lose! Game over.")
                              (newline)
                              updated-state)
                            ;; Neither win nor lose, continue game
                            updated-state)))))
        (begin
          (display "Invalid move.")
          (newline)
          state))))


;; primative functions for now, will need to update state and restart game in future 

(define (win? state)
  (let ((buckets (get-bucket state)))
    (andmap (lambda (bucket-value) (= bucket-value 21)) buckets)))

(define (lose? state)
  (let ((buckets (get-bucket state)))
    (ormap (lambda (bucket-value) (> bucket-value 21)) buckets)))




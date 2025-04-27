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
      (begin
        (display "You cannot pick a card.")
        (newline)
        state)))

(define (place-card state bucket-number)
  (let ((hand (cdr (get-player-hand state)))
        (buckets (get-bucket state))
        (valid-move? (umpire state "place" bucket-number)))
    (if valid-move?
        (let* ((card (car hand))
               (new-hand (cdr hand))
               (bucket-index (- bucket-number 1))
               (card-value (card-value card))
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
          updated-state)
        (begin
          (display "You cannot place. Try a different command.")
          (newline)
          state))))

(define (place-card state bucket-number)
  (let* ((hand (cdr (get-player-hand state)))
         (buckets (get-bucket state))
         (cards-in-bucket (get-cards-in-bucket state))
         (valid-move? (umpire state "place" bucket-number)))
    (if valid-move?
        (let* ((card (car hand))
               (new-hand (cdr hand))
               (bucket-index (- bucket-number 1))
               (current-cards (list-ref cards-in-bucket bucket-index))
               (new-cards (cons card current-cards)) ; Add the card to the list of cards
               (updated-cards-in-bucket (update-cards-in-bucket state bucket-index new-cards)) ; Update cards in bucket
               (bucket-score (calculate-bucket-score new-cards)) ; Calculate the bucket score
               (state-with-new-bucket (update-bucket-value updated-cards-in-bucket bucket-index bucket-score)) ; Update the bucket value
               (updated-state (update-player-hand state-with-new-bucket new-hand))) ; Update the hand

          (display "Card placed: ")
          (display card)
          (newline)
          (display "New hand: ")
          (display (cdr (get-player-hand updated-state)))
          (newline)
          (display "Updated buckets: ")
          (display (get-bucket updated-state))
          (newline)

          updated-state)
        (begin
          (display "Invalid move.")
          (newline)
          state))))


(define (calculate-bucket-score cards)
  (let loop ((cards cards) (score 0) (num-aces 0))
    (cond ((null? cards)
           (display "No more cards. Final score: ")
           (display score)
           (newline)
           (cond ((> score 21)
                  (display "Score > 21, adjusting Aces...\n")
                  (adjust-aces cards score num-aces))
                 (else
                  (display "Score <= 21, returning score.\n")
                  score)))
          ((eq? (car(car cards)) 'a) ; It's an Ace
           (display "Found an Ace, adding 11 to score.\n")
           (loop (cdr cards) (+ score 11) (+ num-aces 1)))
          (else
           (display (car cards))
           (display "Found a non-Ace")
           (loop (cdr cards) (+ score (card-value (car cards))) num-aces)))))

(define (adjust-aces cards score num-aces)
  (display "Adjusting Aces...\n")
  (cond ((or (<= score 21) (= num-aces 0))
         (display "Score <= 21 or no more Aces, returning score.\n")
         score)
        (else
         (display "Score > 21 and Aces remain, subtracting 10 and recursing.\n")
         (adjust-aces cards (- score 10) (- num-aces 1)))))


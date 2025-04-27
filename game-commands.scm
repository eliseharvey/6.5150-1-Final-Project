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
          (display "Updated bucket values: ")
          (display (get-bucket updated-state))
          (newline)
          (display "Cards in buckets:")
          (display (get-cards-in-bucket updated-state))
          (newline)

          updated-state)
        (begin
          (display "Invalid move.")
          (newline)
          state))))



;;; Helper Functions for Ace as 1 or 11 ;;;  
(define (calculate-bucket-score cards)
  (let loop ((cards cards) (score 0) (num-aces 0))
    (cond ((null? cards)
           (cond ((> score 21)
                  (adjust-aces cards score num-aces))
                 (else
                  score)))
          ((eq? (car(car cards)) 'a) ; It's an Ace
           (loop (cdr cards) (+ score 11) (+ num-aces 1)))
          (else
           (loop (cdr cards) (+ score (card-value (car cards))) num-aces)))))

(define (adjust-aces cards score num-aces)
  (cond ((or (<= score 21) (= num-aces 0))
         score)
        (else
         (adjust-aces cards (- score 10) (- num-aces 1)))))


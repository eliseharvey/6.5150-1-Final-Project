;; game-commands.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;       Game Command Actions     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (pick-card state) -> state
;; checks if the move is valid with the umpire, and either returns the unchanged state or the new state with the move executed
(define (pick-card state)
  (if (umpire state "pick")
      (update-with-pick state)
      (begin
        (display "You cannot pick a card.")
        (newline)
        state)))


;; (update-with-pick state) -> updated-state
;; draws a card from the deck and adds it to the player's hand
(define (update-with-pick state)
  (let* ((deck (cdr (get-deck state)))
         (hand (cdr (get-player-hand state)))
         (card (car deck)) ; Get the top card
         (new-deck (cdr deck)) ; Remove top card from deck
         (new-hand (cons card hand))) ; Add card to hand
            (display "Card picked: ")
            (display card)
            (newline)
            (display "New hand: ")
            (display new-hand)
            (newline)
            ;; Create updated state
            (let ((updated-state (update-deck state new-deck)))
              (update-player-hand updated-state new-hand))))

;; (place-card state) -> state
;; checks if the move is valid with the umpire, and either returns the unchanged state or the new state with the move executed 
(define (place-card state bucket-number)
  (if (umpire state "place" bucket-number)
      (update-with-place state bucket-number)
      (begin
        (display "You cannot place a card.")
        (newline)
        state)))

;; (update-with-place state) -> updated-state
;; places the card in the given bucket number
(define (update-with-place state bucket-number)
  (let* ((hand (cdr (get-player-hand state)))
         (buckets (get-bucket state))
         (cards-in-bucket (get-cards-in-bucket state))
         (card (car hand))
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
    updated-state))
       

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;     Bucket Scoring Helpers     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (calculate-bucket-score cards) -> score
;; calculates total score of cards in a bucket, treating Aces as 11 by default
;; adjusts Aces to 1 as needed to avoid busting over 21
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


;; (adjust-aces cards score num-aces) -> adjusted-score
;; recursively reduces score by 10 for each Ace if score > 21
(define (adjust-aces cards score num-aces)
  (cond ((or (<= score 21) (= num-aces 0))
         score)
        (else
         (adjust-aces cards (- score 10) (- num-aces 1)))))


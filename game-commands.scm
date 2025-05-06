;; game-commands.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;       Game Command Actions     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; (player-hit state) -> state
;; draws a card from the deck and adds it to the player's hand
(define (player-hit state)
  (if (not (null? (cdr (get-deck state))))
      (update-player-hit state)
      (begin
        (display "No more cards in the deck!\n")
        state)))

;; (update-player-hit state) -> updated-state
;; draws a card from the deck and adds it to the player's hand
(define (update-player-hit state)
  (let* ((deck (cdr (get-deck state)))
         (hand (get-player-hand state))
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


(define (dealer-play state)
  (let ((dealer-hand (get-dealer-hand state)))
    (if (>= (calculate-hand-score dealer-hand) 17)
        (begin
          (display "Score: ")
          (display (calculate-hand-score dealer-hand))
          (newline)
          state)
        (let* ((deck (cdr (get-deck state)))
               (card (car deck))
               (new-deck (cdr deck))
               (new-dealer-hand (cons card dealer-hand))
               (updated-state (update-deck state new-deck))
               (final-state (update-dealer-hand updated-state new-dealer-hand)))

          (dealer-play final-state)))))
       

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;     Bucket Scoring Helpers     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (calculate-bucket-score cards) -> score
;; calculates total score of cards in a bucket, treating Aces as 11 by default
;; adjusts Aces to 1 as needed to avoid busting over 21
(define (calculate-hand-score hand)
  (let loop ((cards hand) (score 0) (num-aces 0))
    (cond ((null? cards)
           (display "No more cards. Final score: ")
           (display score)
           (newline)
           (if (> score 21)
               (adjust-aces hand score num-aces)
               score))
          ((eq? (car cards) 'a) ; Corrected Ace check
           (loop (cdr cards) (+ score 11) (+ num-aces 1)))
          (else
           (let ((card-val (card-value (car cards))))
             (loop (cdr cards) (+ score card-val) num-aces))))))

;; (adjust-aces cards score num-aces) -> adjusted-score
;; recursively reduces score by 10 for each Ace if score > 21
(define (adjust-aces cards score num-aces)
  (cond ((or (<= score 21) (= num-aces 0))
         (display "Score <= 21 or no more Aces, returning score.\n")
         score)
        (else
         (display "Score > 21 and Aces remain, subtracting 10 and recursing.\n")
         (adjust-aces cards (- score 10) (- num-aces 1)))))




;;; HELPER FUNCTIONS: 
(define (player-bust? state)
  (> (calculate-hand-score (get-player-hand state)) 21))

(define (dealer-bust? state)
  (> (calculate-hand-score (get-dealer-hand state)) 21))


(define (determine-winner state)
  (display  (get-player-hand state))
  (display  (get-dealer-hand state))
  (newline)

  (let ((player-score (calculate-hand-score  (get-player-hand state)))
        (dealer-score (calculate-hand-score  (get-dealer-hand state))))
    (cond ((player-bust? state)
           (display "You busted, Dealer Wins!\n")
           (display "Dealer's Hand: ")
           (display  (get-dealer-hand state))
           (newline))
          ((dealer-bust? state)
           (display "Dealer busted, You Win!\n")
           (display "Dealer's Hand: ")
           (display (get-dealer-hand state))
           (newline))
          ((> player-score dealer-score)
           (display "You Win!\n")
           (display "Dealer's Hand: ")
           (display  (get-dealer-hand state))
           (newline))
          ((< player-score dealer-score)
           (display "Dealer Wins!\n")
           (display "Dealer's Hand: ")
           (display  (get-dealer-hand state))
           (newline))
          (else
           (display "It's a tie!\n")
           (display "Dealer's Hand: ")
           (display  (get-dealer-hand state))
           (newline)))))

(define (adjust-aces cards score num-aces)
  (cond ((or (<= score 21) (= num-aces 0))
         score)
        (else
         (adjust-aces cards (- score 10) (- num-aces 1)))))
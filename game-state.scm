;;; game-state.scm


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;    Game State Construction    ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (make-game-state deck hand bucket) -> game-state-alist
;; constructs a new game state object with players, deck, current turn and history
(define (make-game-state deck player-hand dealer-hand)
  `((deck . ,deck)
    (player-hand . ,player-hand)
    (dealer-hand . ,dealer-hand)))
;; to initialize game, call (make-game-state (make-deck) '() '())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;        Accessor Helpers       ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; returns the current deck from the state
(define (get-deck state)
  (assoc 'deck state))

;; (get-player-hand state) -> list-of-cards
;; retrieves the player's hand
(define (get-player-hand state)
  (cdr (assoc 'player-hand state)))

(define (get-dealer-hand state)
  (cdr (assoc 'dealer-hand state)))

(define (display-dealer-hand state)
  (car (get-dealer-hand state)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Update Helpers (pure/immutable)   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (assoc-set alist key val) -> new-alist
;; returns a new alist with the key set to val (immutable update)
;; helper for updating game states (see helpers below) 
(define (assoc-set alist key val)
  (cons (cons key val)
        (filter (lambda (kv) (not (eq? (car kv) key))) alist)))

;; (update-player-hand state new-hand) -> updated-state
;; replace current hand with hand passed in
(define (update-player-hand state new-hand)
  (assoc-set state 'player-hand new-hand))

;; (update-deck state new-deck) -> updated-state
;; replaces the deck in the game state
(define (update-deck state new-deck)
  (assoc-set state 'deck new-deck))

(define (update-dealer-hand state new-dealer-hand)
  (assoc-set state 'dealer-hand new-dealer-hand))

(define (replace-nth lst n new-val)
   (if (null? lst)
       '()
       (if (= n 0)
           (cons new-val (cdr lst))
           (cons (car lst) (replace-nth (cdr lst) (- n 1) new-val)))))

(define (update-bucket-value state index new-value)
  (let ((buckets (get-bucket state)))
    (assoc-set state 'bucket (replace-nth buckets index new-value))))

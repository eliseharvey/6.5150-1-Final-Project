;;; game-state.scm

#|
For now, leaving a temporary player construction to help with the game state. We may need to update this later when we get to that point

A lot of these are more to help imagine how we could piece together the whole project.
|#

(load "/path_to/cards.scm")
;; for Elise, path to ~/Desktop/6.5150_1 Final Project/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;    Game State Construction    ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (make-player name hand) -> player-alist
;; Creates a player record with a name and starting hand
(define (make-player name hand)
  `((name . ,name)
    (hand . ,hand)))

;; (make-game-state players deck current-player turn history) -> game-state-alist
;; Constructs a new game state object with players, deck, current turn and history
(define (make-game-state players deck current-player turn history)
  `((players . ,players)
    (deck . ,deck)
    (current-player . ,current-player)
    (turn . ,turn)
    (history . ,history)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;        Accessor Helpers       ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns the full list of players
(define (get-players state)
  (assoc 'players state))

;; Returns the current deck from the state
(define (get-deck state)
  (assoc 'deck state))

;; (get-player-hand state name) -> list-of-cards
;; Retrieves a player's hand by name
(define (get-player-hand state name)
  (let ((players (cdr (get-players state))))
    (cdr (assoc 'hand (assoc name (map (lambda (p) (cons (cdr (assoc 'name p)) p)) players))))))

;; Gets the current turn number from the state
(define (get-turn state)
  (cdr (assoc 'turn state)))

;; Gets the symbol of the player whose turn it currently is
(define (get-current-player state)
  (cdr (assoc 'current-player state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Update Helpers (pure/immutable)   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (assoc-set alist key val) -> new-alist
;; Returns a new alist with the key set to val (immutable update)
(define (assoc-set alist key val)
  (cons (cons key val)
        (remove (lambda (kv) (eq? (car kv) key)) alist)))

;; (update-player-hand state player-name new-hand) -> updated-state
;; Replaces the hand of the specified player with a new one
(define (update-player-hand state player-name new-hand)
  (let* ((players (cdr (get-players state)))
         (updated-players
          (map (lambda (p)
                 (if (equal? (cdr (assoc 'name p)) player-name)
                     (assoc-set p 'hand new-hand)
                     p))
               players)))
    (assoc-set state 'players updated-players)))

;; (update-deck state new-deck) -> updated-state
;; Replaces the deck in the game state
(define (update-deck state new-deck)
  (assoc-set state 'deck new-deck))

;; Increments the turn counter by 1
(define (advance-turn state)
  (assoc-set state 'turn (+ 1 (get-turn state))))

;; Sets the current player by name
(define (set-current-player state name)
  (assoc-set state 'current-player name))

;; Adds a new entry to the game history (prepend)
(define (add-to-history state entry)
  (let ((history (cdr (assoc 'history state))))
    (assoc-set state 'history (cons entry history))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;      Player Manipulation      ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-player state name hand) -> updated-state
;; Adds a new player to the game state
(define (add-player state name hand)
  (let* ((players (cdr (get-players state)))
         (new-player (make-player name hand)))
    (assoc-set state 'players (cons new-player players))))

;; Returns a list of all player names
(define (get-player-names state)
  (map (lambda (p) (cdr (assoc 'name p))) (cdr (get-players state))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;    Debug / Print Utilities    ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (print-game-state state) -> void
;; Nicely prints out the current game state to the console
;; Relies on (card->string card) from cards.scm
(define (print-game-state state)
  (display "=== Game State ===\n")
  (for-each
   (lambda (p)
     (display (string-append (symbol->string (cdr (assoc 'name p))) "'s hand: "))
     (display (map card->string (cdr (assoc 'hand p))))
     (newline))
   (cdr (get-players state)))
  (display (string-append "Deck size: " (number->string (length (cdr (get-deck state)))) "\n"))
  (display (string-append "Current turn: " (number->string (get-turn state)) "\n"))
  (display (string-append "Current player: " (symbol->string (get-current-player state)) "\n"))
  (display "==================\n"))

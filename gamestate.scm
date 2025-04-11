;;; game-state.scm


(load "/path_to/cards.scm")
;; for Elise, path to ~/Desktop/6.5150_1 Final Project/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;    Game State Construction    ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (make-game-state deck hand bucket) -> game-state-alist
;; constructs a new game state object with players, deck, current turn and history
(define (make-game-state deck hand bucket)
  `((deck . ,deck)
    (hand . ,hand)
    (bucket . ,bucket)))
;; to initialize game, call (make-game-state make-deck (list '()) (list '() '() '() '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;        Accessor Helpers       ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; returns the current deck from the state
(define (get-deck state)
  (assoc 'deck state))

;; (get-player-hand state name) -> list-of-cards
;; retrieves the player's hand
(define (get-player-hand state)
  (assoc 'hand state)
)
;; (get-bucket state) -> list-of-buckets
;; retrives the current "stacks" in the bucket
(define (get-bucket state)
  (cdr (assoc 'bucket state)))

;; (get-stack state index) -> list with stack
;; gets stack based on index
(define (get-stack state index)
  (list-ref (get-bucket state) index))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Update Helpers (pure/immutable)   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (assoc-set alist key val) -> new-alist
;; returns a new alist with the key set to val (immutable update)
;; helper for updating game states (see helpers below) 
(define (assoc-set alist key val)
  (cons (cons key val)
        (remove (lambda (kv) (eq? (car kv) key)) alist)))

;; (update-player-hand state new-hand) -> updated-state
;; replace current hand with hand passed in
(define (update-player-hand state new-hand)
  (assoc-set state 'hand new-hand))

;; (update-deck state new-deck) -> updated-state
;; replaces the deck in the game state
;; TODO: Eli writes better update deck
(define (update-deck state new-deck)
  (assoc-set state 'deck new-deck))

;; helper for updating stack (see below)
(define (replace-nth lst n new-val)
  (if (null? lst)
      '()
      (if (= n 0)
          (cons new-val (cdr lst))
          (cons (car lst) (replace-nth (cdr lst) (- n 1) new-val)))))

;; (update stack state index new-stack) -> updated-state
;; helper that will take in game state and replace the stack at index with new-stack
(define (update-stack state index new-stack)
  (let ((bucket (get-bucket state)))
    (assoc-set state 'bucket (replace-nth bucket index new-stack))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;    Debug / Print Utilities    ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (print-game-state state) -> void
;; nicely prints out the current game state to the console
;; relies on (card->string card) from cards.scm
(define (print-game-state state)
  (display "=== Game State ===\n")
  ;; hand
  (display "Current hand: ")
  (display (map card->string (cdr (assoc 'hand state))))
  (newline)
  (newline)
  ;; stacks
  (display "Stacks:\n")
  (let ((bucket (cdr (assoc 'bucket state))))
    (for-each-indexed
     (lambda (stack i)
       (display (string-append "  Stack " (number->string i) ": "))
       (display (map card->string stack))
       (newline))
     bucket))
  (newline)
  ;; deck
  (display (string-append "Deck size: "
                          (number->string (length (cdr (assoc 'deck state))))
                          "\n"))
  (display "==================\n"))

;; Helper: for-each-indexed
(define (for-each-indexed f lst)
  (define (loop lst i)
    (unless (null? lst)
      (begin
        (f (car lst) i)
        (loop (cdr lst) (+ i 1)))))
  (loop lst 0))

;; play-game.scm

#|
;; Eli

(load "/Users/eli/6.5150/6.5150-1-Final-Project/cards.scm")
(load "/Users/eli/6.5150/6.5150-1-Final-Project/game-state.scm")
(load "/Users/eli/6.5150/6.5150-1-Final-Project/utils.scm")
(load "/Users/eli/6.5150/6.5150-1-Final-Project/game-commands.scm")
(load "/Users/eli/6.5150/6.5150-1-Final-Project/umpire.scm")
|#


#|
;; Elise

(load "~/Desktop/6.5150-1-Final-Project/cards.scm")
(load "~/Desktop/6.5150-1-Final-Project/game-state.scm")
(load "~/Desktop/6.5150-1-Final-Project/utils.scm")
(load "~/Desktop/6.5150-1-Final-Project/game-commands.scm")
(load "~/Desktop/6.5150-1-Final-Project/umpire.scm")
|#


#|
To play:
   1. Load this file.
   2. In the REPL, start game with (play-game).
|#

(define game-state '())


;; (play-game) -> void
;; initializes a new game and sets the global `game-state` variable.
(define (play-game)
  (set! game-state (make-game-state (make-deck) '() '(0 0 0 0) '(() () () ())))
  (display "Welcome to Stack 21!\n")
  (print-game-state game-state)
  (display "Commands:\n")
  (display "  (pick)        ; Draw a card from the deck\n")
  (display "  (place n)     ; Place top card of hand into stack n (1-4)\n")
  (display "  (print-game-state) ; Reprint current game state\n"))


;; (pick) -> void
;; wraps the pick-card from game-commands.scm
(define (pick)
  (set! game-state (pick-card game-state))
  (print-game-state game-state))


;; (place n) -> void
;; wraps the place-card from game-commands.scm
(define (place n)
  (set! game-state (place-card game-state n))
  (set! game-state (game-ended? game-state))
  (print-game-state game-state))

;; (game-ended? state) -> game-state
;; checks if the game is won (any stack = 21) or lost (any stack > 21),
(define (game-ended? state)
  (let ((buckets (cdr (assoc 'bucket state))))
    (cond
     ;; loss if any bucket > 21
     ((any? (lambda (x) (> x 21)) buckets)
      (display "You lose! A stack went over 21!\n")
      (display "Game has been reset. Play again!\n")
      (make-game-state (make-deck) '() '(0 0 0 0) '(() () () ())))
     ;; win any bucket = 21
     ((any? (lambda (x) (= x 21)) buckets)
      (display "You win! One of your stacks hit 21 exactly!\n")
      (display "Game has been reset. Play again!\n")
      (make-game-state (make-deck) '() '(0 0 0 0) '(() () () ())))
     ;; continue
     (else state))))


;; (card->string-simple card) -> string
;; converts a card (list 'card rank suit) into a simple string
(define (card->string-simple card)
  (if (and (pair? card)
           (eq? (car card) 'card)
           (pair? (cdr card))
           (pair? (cddr card)))
      (string-append (symbol->string (cadr card)) " of " (symbol->string (caddr card)))
      "<invalid card>"))

      
;; (print-game-state state) -> void
;; nicely prints the current game state (hand, stacks, and deck size).
(define (print-game-state state)
  (display "=== Game State ===\n")
  ;; Print buckets
  (display "Buckets:\n")
  (let ((buckets (cdr (assoc 'bucket state))))
    (let loop ((buckets buckets) (i 1))
      (unless (null? buckets)
        (display (string-append "  Stack " (number->string i) ": "))
        (display (number->string (car buckets)))
        (newline)
        (loop (cdr buckets) (+ i 1)))))
  (newline)
  ;; print deck size
  (let ((deck (cdr (assoc 'deck state))))
    (display (string-append "Deck size: " (number->string (length deck)) "\n")))
  (display "==================\n"))

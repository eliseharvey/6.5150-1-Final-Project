(load "path-to/6.5150-1-Final-Project/cards.scm")
(load "path-to/6.5150-1-Final-Project/game-state.scm")
(load "path-to/6.5150-1-Final-Project/utils.scm")
(load "path-to/6.5150-1-Final-Project/game-commands.scm")

;; Initialize a game
(define game-state (make-game-state (make-deck) '() '(0 0 0 0)))

(set! game-state (pick-card game-state))

(set! game-state (place-card game-state 2))

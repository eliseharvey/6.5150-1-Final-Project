;; test-game.scm

#|
;; Eli

(load "/Users/eli/6.5150/6.5150-1-Final-Project/cards.scm")
(load "/Users/eli/6.5150/6.5150-1-Final-Project/game-state.scm")
(load "/Users/eli/6.5150/6.5150-1-Final-Project/utils.scm")
(load "/Users/eli/6.5150/6.5150-1-Final-Project/game-commands.scm")
(load "/Users/eli/6.5150/6.5150-1-Final-Project/umpire.scm")
(load "/Users/eli/6.5150/6.5150-1-Final-Project/play-game.scm") 
|#


#|
;; Elise

(load "~/Desktop/6.5150-1-Final-Project/cards.scm")
(load "~/Desktop/6.5150-1-Final-Project/game-state.scm")
(load "~/Desktop/6.5150-1-Final-Project/utils.scm")
(load "~/Desktop/6.5150-1-Final-Project/game-commands.scm")
(load "~/Desktop/6.5150-1-Final-Project/umpire.scm")
(load "~/Desktop/6.5150-1-Final-Project/play-game.scm") 
|#


#|
To test, simply load this file after uncommenting related loads above.
|#

;; for readability
(define (print-divider)
  (newline)
  (newline)
  (display "==============================\n")
  (newline)
  (newline))

;; test: starting gmae
(display "TEST 1: Initializing game...\n")
(define game-state (make-game-state (make-deck) '() '(0 0 0 0) '(() () () ())))
(print-game-state game-state)
(print-divider)

;; test: picking card
(display "TEST 2: Picking a card...\n")
(set! game-state (pick-card game-state))
(print-game-state game-state)
(print-divider)

;; test: placing card
(display "TEST 3: Attempting to place card into Stack 2...\n")
(let ((valid? (umpire game-state "place" 2)))
  (display (if valid?
               "Valid move. Proceeding...\n"
               "Invalid move. Should not place.\n"))
  (if valid?
      (begin
        (set! game-state (place-card game-state 2))
        (print-game-state game-state))
      (display "Skipping placement due to invalid move.\n")))
(print-divider)

;; test: losing game
(display "TEST 4: Testing a loss...\n")
(define stacked-state
  (make-game-state
   '()
   '()
   '(22 0 0 0)
   '((card 10 hearts card 12 diamonds card a clubs) () () ())))

(print-game-state stacked-state)
(define end-result (game-ended? stacked-state))
(print-game-state end-result)
(print-divider)

;; test: winning game
(display "TEST 5: Testing a loss...\n")
(define win-state
  (make-game-state
   '()
   '()
   '(21 21 21 21)
   '((card 10 hearts card a spades) 
     (card q diamonds card j clubs card a hearts)
     (card 9 clubs card 2 diamonds card 10 hearts)
     (card 7 spades card 7 clubs card 7 hearts))))

(print-game-state win-state)
(define win-result (game-ended? win-state))
(print-game-state win-result)
(print-divider)

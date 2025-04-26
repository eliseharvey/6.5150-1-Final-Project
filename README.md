# 6.5150/1 Final Project: Composable Framework for Card Games in MIT/GNU Scheme
### By Eli Scharf and Elise Harvey

---

This project aims to explore turn-based card games like Blackjack, War, and even Poker. Our goal is to build a flexible framework using MIT/GNU Scheme that is composable into various card games without needing a new code base. To do this, we implemented reusable components like players, cards, decks, hands, and rules that would enable the system to play different games by recomposing existing components.  

To make this feasible in our time allotment, we created a game called *Stack 21* and implemented it using our card game components. The goal of the game is to build a stack that sums to 21, starting with four empty stacks. The player will alternate picking and placing cards until they either win (get a stack that sums exactly to 21) or they lose (a stack goes ovr 21).

_ELI ADD AMB/ACES THING_

---

### How to Play

1. Load `cards.scm`, `game-state.scm`, `game-commands.scm`, `play-game.scm`, `umpire.scm`, and `utils.scm`.
    - Alternatively, make sure the load statements at the top of `play-game.scm` are correct and only load that file.
2. Enter `(play-game)` in the REPL and start playing!
3. To pick, enter `(pick)`.
4. To place, enter `(place n)` where `n` is a stack, 1 to 4.

---

### File Structure

1. `cards.scm`
    - Where the card and deck logic is defined. Sets up a deck with 52 cards, 4 of each rank in each suit. 
2. `game-commands.scm`
    - This is where the work gets done when the player makes a move. It makes a call to `umpire.scm` to make sure the move is valid given the game state and desired move. 
3. `game-state.scm`
    - Where the game state data structure is defined. Contains any necessary helpers like `make-game-state`, `get-deck`, etc.
4. `play-game.scm`
    - The wrapper that enables the player to play the game! It wraps commands and tracks the game state to enable playing in the REPL. Also includes a text-based UI that prints the game state on each turn. It also contains the logic for when the game is over (and resets the game once won/lost).
5. `test-game.scm`
    - Testing file to test game moves.
6. `umpire.scm`
    - The umpire takes a game state and move and determines whether the move is valid. As the umpire takes in rules like predicates, changing the rule logic will change how the game is played.
7. `utils.scm`
    - Contains any additional helper functions. 
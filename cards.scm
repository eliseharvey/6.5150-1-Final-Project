;; cards.scm
;; generic card maker --can be expanded in future to make more interesting decks 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;          Card Helpers         ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (make-card rank suit) -> card
;; creates a basic card representation (expandable in future for complex cards)
(define (make-card rank suit)
  (list 'card rank suit))


;; predefined ranks and suits
(define ranks '(2 3 4 5 6 7 8 9 10 j q k a))
(define suits '(hearts diamonds clubs spades))

#|
;; Extensible cartesian product function for more than 2 lists
(define (cartesian-product lists)
  (if (null? lists)
      (list '()) 
      (let ((first-list (car lists))
            (rest-lists (cdr lists)))
        (append-map (lambda (x)
                      (map (lambda (y) (cons x y))
                           (cartesian-product rest-lists)))
                    first-list))))
|# 


;; (cartesian-product lst1 lst2) -> list-of-pairs
;; returns the cartesian product of two lists... used to build generic deck
(define (cartesian-product lst1 lst2)
  (if (null? lst1)
      '()
      (append-map (lambda (x) (map (lambda (y) (list x y)) lst2)) lst1)))


;; (make-deck) -> shuffled-deck
;; creates and shuffles a standard 52-card deck
(define (make-deck)
  (shuffle (cartesian-product ranks suits)))


;; (make-custom-deck custom-ranks custom-suits) -> shuffled-deck
;; creates and shuffles a deck using provided ranks and suits
(define (make-custom-deck custom-ranks custom-suits)
  (shuffle (cartesian-product custom-ranks custom-suits)))


;; (shuffle deck) -> shuffled-deck
;; randomly shuffles a deck
(define (shuffle deck)
  (define (random-index n) (random n))

  (define (shuffle-helper deck result)
    (if (null? deck)
        result
        (let ((i (random-index (length deck))))
          (shuffle-helper (remove-i-th deck i) (cons (list-ref deck i) result)))))
  (shuffle-helper deck '()))


;; (remove-i-th lst i) -> list
;; removes the ith element from a list
(define (remove-i-th lst i)
  (append (take lst i) (drop lst (+ i 1))))


;; (deal-cards deck num-cards) -> (hand new-deck)
;; deals num-cards from deck, returns hand and remaining deck
(define (deal-cards deck num-cards)
  (if (<= num-cards 0)
      (values '() deck)
      (let ((card (car deck)))
        (let-values (((hand new-deck) (deal-cards (cdr deck) (- num-cards 1))))
          (values (cons card hand) new-deck)))))


;; (draw-card deck) -> card
;; draws the top card from the deck (no removal)
(define (draw-card deck)
  (if (null? deck)
      '()  
      (car deck))) 


;; (remove item lst all?) -> list
;; removes first occurrence (all? = #f) or all occurrences (all? = #t) of item
(define (remove item lst all?)
  (cond ((null? lst) '())
        ((equal? (car lst) item)
         (if all?
             (remove item (cdr lst) all?)  
             (cdr lst)))                   
        (else (cons (car lst) (remove item (cdr lst) all?)))))


;; (remove-first item lst) -> list
;; removes the first occurrence of item from the list
(define (remove-first item lst)
  (remove item lst #f))


;; (remove-all item lst) -> list
;; removes all occurrences of item from the list
(define (remove-all item lst)
  (remove item lst #t))


;; (discard-first-occur deck card) -> deck
;; removes the first occurrence of card from deck
(define (discard-first-occur deck card)
  (remove-first card deck))


;; (discard-all-occur deck card) -> deck
;; removes all occurrences of card from deck
(define (discard-all-occur deck card)
  (remove-all card deck))


;; (card-value card) -> number
;; returns numeric value of a card (ace = 11, face cards = 10, number cards = value)
(define (card-value card)
  (let ((rank (if (list? (car card)) (car (car card)) (car card))))
    (cond ((eq? rank 'a) 11)
          ((member rank '(k q j)) 10)
          ((number? rank) rank)
          (else 0))))

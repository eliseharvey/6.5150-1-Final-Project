;; generic card maker --can be expanded in future to make more interesting decks 

(define (make-card rank suit)
  (list 'card rank suit))

(define ranks '(2 3 4 5 6 7 8 9 10 'J 'Q 'K 'A))
(define suits '(hearts diamonds clubs spades))

(define (cartesian-product lst1 lst2)
  (if (null? lst1)
      '()
      (append-map (lambda (x) (map (lambda (y) (list x y)) lst2)) lst1)))

(define (make-deck)
  (shuffle (cartesian-product ranks suits)))



(define (shuffle deck)
  (define (random-index n) (random n))

  (define (shuffle-helper deck result)
    (if (null? deck)
        result
        (let ((i (random-index (length deck))))
          (shuffle-helper (remove-i-th deck i) (cons (list-ref deck i) result)))))
  (shuffle-helper deck '()))


(define (remove-i-th lst i)
  (append (take lst i) (drop lst (+ i 1))))


(define (deal-cards deck num-cards)
  (if (<= num-cards 0)
      (values '() deck)
      (let ((card (car deck)))
        (let-values (((hand new-deck) (deal-cards (cdr deck) (- num-cards 1))))
          (values (cons card hand) new-deck)))))


(define (draw-card deck)
  (if (null? deck)
      '()  ;; Return an empty list if the deck is empty
      (car deck))) 

(define (discard-card deck card)
  (remove card deck))

(define (make-custom-deck custom-ranks custom-suits)
  (shuffle (cartesian-product custom-ranks custom-suits)))













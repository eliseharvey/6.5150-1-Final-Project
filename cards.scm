;; generic card maker --can be expanded in future to make more interesting decks 

(define (make-card rank suit)
  (list 'card rank suit))

(define ranks '(2 3 4 5 6 7 8 9 10 j q k a))
(define ranks '(2 k a))
(define suits '(hearts diamonds clubs spades))

(define (cartesian-product lst1 lst2)
  (if (null? lst1)
      '()
      (append-map (lambda (x) (map (lambda (y) (list x y)) lst2)) lst1)))

(define (make-deck)
  (shuffle (cartesian-product ranks suits)))

(define (make-custom-deck custom-ranks custom-suits)
  (shuffle (cartesian-product custom-ranks custom-suits)))


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
      '()  
      (car deck))) 

(define (remove item lst all?)
  (cond ((null? lst) '())
        ((equal? (car lst) item)
         (if all?
             (remove item (cdr lst) all?)  
             (cdr lst)))                   
        (else (cons (car lst) (remove item (cdr lst) all?)))))

(define (remove-first item lst)
  (remove item lst #f))

(define (remove-all item lst)
  (remove item lst #t))

(define (discard-first-occur deck card)
  (remove-first card deck))

(define (discard-all-occur deck card)
  (remove-all card deck))


(define (card-value card)
  (let ((rank (if (list? (car card)) (car (car card)) (car card))))
    (cond ((eq? rank 'a) 11)
          ((member rank '(k q j)) 10)
          ((number? rank) rank)
          (else 0))))
(define (andmap pred lst)
  (cond ((null? lst) #t)
        ((pred (car lst)) (andmap pred (cdr lst)))
        (else #f)))

(define (ormap pred lst)
  (cond ((null? lst) #f)
        ((pred (car lst)) #t)
        (else (ormap pred (cdr lst)))))
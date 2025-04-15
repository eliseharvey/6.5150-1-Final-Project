;; utils.scm


(define (any? pred lst)
  (cond
    ((null? lst) #f)
    ((pred (car lst)) #t)
    (else (any? pred (cdr lst)))))

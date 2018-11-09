#lang racket

(define (rember* a l)
  (cond
    [(null? l) '()]
    [(pair? (car l)) (cons (rember* a (car l))
                           (rember* a (cdr l)))]
    [(eqv? a (car l)) (rember* a (cdr l))]
    [else (cons (car l)
                (rember* a (cdr l)))]))



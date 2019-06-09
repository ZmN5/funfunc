#lang racket

(define-syntax foo
  (lambda (stx)
    (syntax "I am foo")))


(define-syntax (also-foo stx)
  (syntax "I am also foo"))

(define-syntax (quoted-foo stx)
  #'"I am quoted foo")

(define-syntax (show-me stx)
  (print stx)
  #'(void))


(define-syntax (reverse-me stx)
  (datum->syntax stx (reverse (cdr (syntax->datum stx)))))

#lang racket

;; cons car cdr

(define (cons-v1 x y)
  (define (dispatch m)
    (cond
      [(= m 0) x]
      [(= m 1) y]
      (else (error "must 0 or 1"))))
  dispatch)

(define (car-v1 z) (z 0))
(define (cdr-v1 z) (z 1))

;; 2.4

(define (cons-v2 x y)
  (lambda (m) (m x y)))

(define (car-v2 z)
  (z (lambda (x y) x)))

(define (cdr-v2 z)
  (z (lambda (x y) y)))


;; 2.5

(define (cons-v3 x y)
  (* (expt 7 x)
     (expt 8 y)))


(define (num-divs num x)
  (define (iter num result)
    (if (= 0 (remainder num x))
        (iter (/ num x)
              (add1 result))
        result))
  (iter num 0))

(define (car-v3 z)
  (num-divs z 7))


(define (cdr-v3 z)
  (num-divs z 8))

;; 2.6

(define zero
  (lambda (f)
    (lambda (x) x)))

(define (add-1 n)
  (lambda (f)
    (lambda (x) (f (n f) x))))

;; 2.17


(define (last-p lst)
  (if (null? (cdr lst))
      (list (car lst))
      (last-p (cdr lst))))


;; 2.18

(define (reverse-v2 lst)
  (define (iter remainder-lst result)
    (if (null? remainder-lst)
        result
        (iter (cdr remainder-lst)
              (cons (car remainder-lst)
                    result))))
  (iter lst '()))

;; 2.20


(define (same-parity x . y)
  (filter (if (even? x) even? odd?)
          (cons x y)))


;; 2.21

(define (square x) (* x x))

(define (square-list-v1 lst)
  (if (null? lst)
      '()
      (cons (square (car lst))
            (square-list-v1 (cdr lst)))))

(define (square-list lst)
  (map square lst))


;; 2.23

(define (foreach func lst)
  (if (not (null? lst))
      (begin
        (func (car lst))
        (foreach func (cdr lst)))
      null))


(define (countleaves tree)
  (cond
    [(null? tree) 0]
    [(not (pair? tree)) 1]
    [else (+ (countleaves (car tree))
             (countleaves (cdr tree)))]))



;; 2.27

(define (binary-tree-reverse tree)
  (cond
    [(null? tree) '()]
    [(not (pair? tree)) tree]
    [else (reverse (list (binary-tree-reverse (car tree))
                         (binary-tree-reverse (cadr tree))))])
  )

(define (tree-reverse tree)
  (define (iter remainder-lst result)
    (if (null? remainder-lst)
        result
        (iter (cdr remainder-lst)
              (cons
                (if (pair? (car remainder-lst))
                  (tree-reverse (car remainder-lst))
                  (car remainder-lst))
                result))))
  (iter tree '()))


;; 2.29

(define (fringe tree)
  (cond
    [(null? tree) '()]
    [(not (pair? tree)) (list tree)]
    [else (append (fringe (car tree))
                  (fringe (cdr tree)))]))

;; 2.30

(define (square-tree tree)
  (cond
    [(null? tree) '()]
    [(not (pair? tree)) (square tree)]
    [else (cons (square-tree (car tree))
                (square-tree (cdr tree)))]))

(define (map-square-tree tree)
  (map
   (lambda (x)
     (if (pair? x)
         (map-square-tree x)
         (square x)))
   tree))

;; 2.31

(define (tree-map func tree)
  (map
   (lambda (x)
     (if (pair? x)
         (tree-map func x)
         (func x)))
   tree))

(define (square-tree-v3 tree)
  (tree-map square tree))

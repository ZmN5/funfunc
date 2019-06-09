#lang racket

(provide atom?)

(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

(define lat?
  (lambda (x)
    (cond
      [(null? x) #t]
      [(atom? (car x)) (lat? (cdr x))]
      [else #f])))


(define member?
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [(= a (car lat)) #t]
      [else (member? a (cdr lat))])))

(define rember
  (lambda (x lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) x) (cdr lat)]
      [else (cons (car lat)
                  (rember x (cdr lat)))])))

(define firsts
  (lambda (lat)
    (cond
      [(null? lat) '()]
      [else (cons (car (car lat))
                  (firsts (cdr lat)))])))

(define insertR
  (lambda (l old new)
    (cond
      [(null? l) '()]
      [(eq? old (car l)) (cons old
                               (cons new (cdr l)))]
      [else (cons (car l)
                  (insertR (cdr l) old new))])))


(define insertL
  (lambda (l old new)
    (cond
      [(null? l) '()]
      [(eq? old (car l)) (cons new
                               l)]
      [else (cons (car l)
                  (insertL (cdr l) old new))])))

(define subst
  (lambda (l old new)
    (cond
      [(null? l) '()]
      [(eq? old (car l)) (cons new
                               (cdr l))]
      [else (cons (car l)
                  (subst (cdr l) old new))])))


(define subst2
  (lambda (l old1 old2 new)
    (cond
      [(null? l) '()]
      [(or (eq? old1 (car l))
            (eq? old2 (car l))) (cons new
                                     (cdr l))]
      [else (cons (car l)
                  (subst2 (cdr l) old1 old2 new))])))


(define multirember
  (lambda (l x)
    (cond
      [(null? l) '()]
      [(eq? x (car l)) (multirember (cdr l) x)]
      [else (cons (car l)
                  (multirember (cdr l) x ))])))



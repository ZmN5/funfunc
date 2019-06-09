#lang racket

(define (escape v)
  (abort-current-continuation
   (default-continuation-prompt-tag)
   (lambda () (* 100 v))))

(define (test-prompt)
  (+ 1
   (call-with-continuation-prompt
    (lambda ()
      (+ 1 (+ 1 (+ 1 (escape 2)))))
    (default-continuation-prompt-tag))))

(define saved-k #f)

(define (save-it!)
  (call-with-composable-continuation
   (lambda (k)
     (set! saved-k k)
     0)))


;; https://www.scheme.com/tspl4/further.html#./further:h0

;; break

(define multi
  (lambda (ls)
    (call/cc
     (lambda (break)
       (let f ([ls ls])
         (cond
           [(null? ls) 1]
           [(zero? (car ls)) (break 0)]
           [else (* (car ls)
                    (f (cdr ls)))]))))))

;; factorial

(define retry #f)

(define factorial
  (lambda (n)
    (cond
      [(= n 0) (call/cc (lambda (k)
                          (set! retry k)
                          1))]
      [else (* n
               (factorial (- n 1)))])))


(define lwp-list '())

(define lwp
  (lambda (thunk)
    (set! lwp-list (append lwp-list (list thunk))))) 

(define start
  (lambda ()
    (let ([p (car lwp-list)])
      (set! lwp-list (cdr lwp-list))
      (p))))

(define pause
  (lambda ()
    (call/cc
     (lambda (k)
       (lwp (lambda () (k #f)))
       (start)))))

(lwp (lambda () (let f () (pause) (display "h") (f))))
;; (lwp (lambda () (let f () (pause) (display "e") (f))))
;; (lwp (lambda () (let f () (pause) (display "y") (f))))
;; (lwp (lambda () (let f () (pause) (display "!") (f))))
;; (lwp (lambda () (let f () (pause) (newline) (f))))

;; https://cgi.soic.indiana.edu/~c311/lib/exe/fetch.php?media=cpslecture.scm


;; (define rember4
;;   (lambda (ls)
;;     (cond
;;       [(null? ls) '()]
;;       [(= 4 (car ls))  (cdr ls)]
;;       [else (cons (car ls)
;;                   (rember4 (cdr ls)))])))

;; TODO

;; https://cgi.soic.indiana.edu/~c311/doku.php?id=cps-refresher

(define fact-cps
  (lambda (n k)
    (cond
      [(zero? n) (k 1)]
      [else (fact-cps (sub1 n)
                      (lambda (v)
                        (k (* n v))))])))

(define fact
  (lambda (n)
    (fact-cps n (lambda (v) v))))

;; TODO

(define (duplicate pos lst)
  (let dup ([i 0]
            [lst lst])
    (cond
      [(= i pos) (cons (car lst) lst)]
      [else (cons (car lst) (dup (+ i 1) (cdr lst)))])))


;; https://www.it.uu.se/edu/course/homepage/avfunpro/ht13/lectures/Racket-2-Continuations.pdf


(define (sum lst cont)
  (cond [(cons? lst)
         (sum (cdr lst)
              (lambda (acc)
                (cont (+ (car lst) acc))))]
        [else (cont 0)]))

(define rember4
  (lambda (lat k)
    (cond
      [(null? lat) (k '())]
      [(= 4 (car lat)) (k (cdr lat))]
      [else (rember4 (cdr lat)
                      (lambda (x)
                        (k (cons (car lat) x))))])))


(define rember4*
  (lambda (lat k)
    (cond
      [(null? lat) (k '())]
      [(= 4 (car lat)) (rember4* (cdr lat)
                                 (lambda (x)
                                   (k x)))]
      [else (rember4* (cdr lat)
                     (lambda (x)
                       (k (cons (car lat) x))))])))


((call/cc
  (lambda (cont) cont))
 (lambda (x) "hi")
  )

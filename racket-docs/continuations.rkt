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

;; https://cgi.soic.indiana.edu/~c311/lib/exe/fetch.php?media=cpslecture.scm


(define rember4
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(= 4 (car ls))  (cdr ls)]
      [else (cons (car ls)
                  (rember4 (cdr ls)))])))

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

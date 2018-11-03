#lang racket

(provide (contract-out [amount positive?]))

(define amount -1)

(define (pt height)
  (if (zero? height)
      (void)
      (begin
        (display (make-string height #\*))
        (newline)
        (pt (sub1 height)))))

(define (pt2 height)
	(cond
		[(positive? height)
			(display (make-string height #\*))
			(newline)
			(pt2 (sub1 height))]))

(define greeted null)
(define (greet name)
    (begin
        (set! greeted (cons name greeted))
		(string-append "hello, " name)
		greeted))

(define test
  (lambda (x)
	(+ x 1)))

(struct posn (x y))
(define p1 (posn 1 2))


;; https://docs.racket-lang.org/guide/prompt.html

(define (escape v)
  (abort-current-continuation
	(default-continuation-prompt-tag)
	(lambda () v)))

(+ 1 
  (call-with-continuation-prompt
  (lambda ()
	(+1 (+1 (+1 (escape 0)))))
  (default-continuation-prompt-tag)))

(define saved-k #f)

(define (saveit!)
  (call-with-composable-continuation
	(lambda (k)
	  (set! saved-k k)
	  0)))

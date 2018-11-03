#lang racket

(define sub1
  (lambda (x)
    (- x 1)))

(define ++
  (lambda (m n)
	(cond
	  [(zero? n) m]
	  [else (add1 (++ m (sub1 n)))])))

(define --
  (lambda (m n)
	(cond
	  [(zero? n)m ]
	  [else (sub1 (-- m (sub1 n)))])))

(define addtup
  (lambda (tup)
	(cond
	  [(null? tup) 0]
	  (else (+ (car tup)
			   (addtup (cdr tup)))))))

(define x
  (lambda (m n)
	(cond
	  [(zero? n) 0]
	  [else (+ m 
			   (x m (sub1 n)))])))

(define tup+
  (lambda (t1 t2)
	(cond
	  [(and (null? t1) (null? t2)) '()]
	  [(null? t1) t2]
	  [(null? t2) t1]
	  [else (cons (+ (car t1) (car t2))
				  (tup+ 
					(cdr t1) (cdr t2)))])))

(define <<
  (lambda (m n)
	(cond 
	  [(zero? n) #f]
	  [(zero? m) #t]
	  [else (<< (sub1 m)
				(sub1 n))])))

(define >> 
  (lambda (m n)
	(cond 
	  [(zero? m) #f]
	  [(zero? n) #t]
	  [else (>> (sub1 m)
				(sub1 n))])))

;; (define === 
;;   (lambda (m n)
;;     (cond 
;;     [(zero? m) (zero? n)]
;; 	  [(zero? n) #f]
;;       [else (=== (sub1 m)
;;         (sub1 n))])))

(define ===
  (lambda (m n)
	(cond 
	  [(< m n) #f]
	  [(> m n) #f]
	  [else #t])))

(define //
  (lambda (m n)
	(cond 
	  [(< m n) 0]
	  [else (add1 (// (- m n) n))])))

(define len
  (lambda (lat)
	(cond
	  [(null? lat) 0]
	  [else (add1 (len (cdr lat)))])))


(define carn
  (lambda (lat n)
	(cond
	  [(zero? n) '()]
	  [else (cons (car lat)
				  (carn (cdr lat)
						(sub1 n)))])))

(define pick
  (lambda (lat n)
	(cond 
	  [(zero? (sub1 n)) (car lat)]
	  (else (pick (cdr lat) (sub1 n))))))

(define rempick
  (lambda (lat n)
	(cond
	  [(zero? (sub1 n)) (cdr lat)]
	  [else (cons (car lat)
				  (rempick (cdr lat)
						   (sub1 n)))])))

(define all-nums
  (lambda (lat)
	(cond
	  [(null? lat) '()]
	  [else (cond
			  [(number? (car lat)) (cons (car lat)
										 (all-nums (cdr lat)))]
			  [else (all-nums (cdr lat))])])))

(define no-nums
  (lambda (lat)
	(cond
	  [(null? lat) '()]
	  [else (cond 
			  [(number? (car lat)) (no-nums (cdr lat))]
			  [else (cons (car lat)
						  (no-nums (cdr lat)))])])))

(define eqan?
  (lambda (m n)
	(cond 
	  [(and (number? m) (number? n)) (= m n)]
	  [(or (number? m) (number? n)) #f]
	  [else (eq? m n)])))

(define occur
  (lambda (lat n)
	(cond
	  [(null? lat) 0]
	  [(= n (car lat)) (add1 (occur (cdr lat) n))]
	  [else (occur (cdr lat) n)])))

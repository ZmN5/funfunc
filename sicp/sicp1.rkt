#lang racket

;; 1.3
(define (max a b)
  (if (< a b)
	b
	a))

(define (sum-max2 a b c)
  (cond
	[(< a b) (+ b (max a c))]
	[(> a b) (+ a (max b c))]))

;; 求平方根

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess	
	(sqrt-iter (improve-guess guess x)
			   x)))

(define (square x) (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess)
			 x))
	 0.001))

(define (average x y)
  (/ (+ x y) 2))

(define (improve-guess guess x)
  (average guess (/ x guess))
  )

(define (sqrt x)
  (sqrt-iter 1 x))

;; 1.7
(define (sqrt-iter-v2 guess x)
  (let [(new-guess (improve-guess guess x))]
  	(if (good-enough-v2? new-guess guess)
    	new-guess	
		(sqrt-iter-v2 new-guess x))))

(define (good-enough-v2? guess pre-guess)
  (< (/ (abs (- guess pre-guess) ) guess) 0.000001))

(define (sqrt-v2 x)
  (sqrt-iter-v2 1.0 x))


;; 1.8

(define (calc-iter imporve-func guess x)
  (let [(new-guess (imporve-func guess x))]
	(if (good-enough-v2? new-guess guess)
	  new-guess
	  (calc-iter imporve-func new-guess x))))

(define (improve-cbrt-guess guess x)
  (/ (+ (/ x (* guess guess))
		(* 2 guess)) 
	 3))

(define (cbrt x)
  (calc-iter improve-cbrt-guess 1.0 x))

(define (sqrt-v3 x)
  (calc-iter improve-guess 1.0 x))


;; factorial

(define (factorial1 x)
  (if (= x 1)
	1
	(* x (factorial1 (sub1 x)))))

(define (factorial2 x)
  (define (factorial-iter result counter)
	(if (= counter 0)
	  result
	  (factorial-iter (* result counter) 
					  (sub1 counter))))
	(factorial-iter 1 x))


;; fibonacci

(define (fib n)
  (cond
	[(= n 0) 1]
	[(= n 1) 1]
	(else (+ (fib (- n 1))
			 (fib (- n 2))))))

(define (fib2 n)
  (define (fib-iter a b counter)
	(if (= 0 counter)
	  b
	  (fib-iter b (+ a b) (sub1 counter))))
  (fib-iter 0 1 n))

;; count change
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond
	[(= amount 0) 1]
	[(or (< amount 0) (= kinds-of-coins 0)) 0]
	[else (+ (cc amount 
				 (sub1 kinds-of-coins))
			 (cc (- amount 
					(first-denomination kinds-of-coins))
				 kinds-of-coins))]))

(define (first-denomination kinds-of-coins)
  (cond
	[(= kinds-of-coins 1) 1]
	[(= kinds-of-coins 2) 5] 
	[(= kinds-of-coins 3) 10] 
	[(= kinds-of-coins 4) 25] 
	[(= kinds-of-coins 5) 50]))

;; 1.11

;; recursion
(define (fibona3-recur n)
  (if (< n 3) 
	n
	(+ (fibona3-recur (- n 1))
	   (* 2 (fibona3-recur (- n 2)))
	   (* 3 (fibona3-recur (- n 3))))))

;; iteration

(define (fibona3-iter n)
  (define (fib-iter a b c counter)
	(if (= counter n)
	  c
	  (fib-iter (+ a (* 2 b) (* 3 c))
				a 
				b
				(add1 counter))))
  (fib-iter 2 1 0 0))


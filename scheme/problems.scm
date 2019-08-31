(define tree-copy
  (lambda (tr)
    (if (not (pair? tr))
        tr
        (cons (tree-copy (car tr))
              (tree-copy (cdr tr))))))


(define lazy
  (lambda (t)
    (let ([val #f] [flag #f])
      (lambda ()
        (if (not flag)
          (begin (set! val (t))
                 (set! flag #t)))
          val))))


(define-syntax let_
  (syntax-rules ()
    [(_ ((x e) ...) b1 b2 ...)
     ((lambda (x ...) b1 b2 ...) e ...)]))


(define fibonacci
  (lambda (n)
    (if (= n 0)
        0
        (let fib ([i n] [a 1] [b 0])
          (if (= i 1)
              a
              (fib (- i 1) (+ a b) a))))))


(define factor
  (lambda (n)
    (trace-let f ([n n] [i 2])
      (cond
       [(>= i n) (list n)]
       [(integer? (/ n i))
        (cons i (f (/ n i) i))]
       [else (f n (+ i 1))])))) 


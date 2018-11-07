#lang racket

(require racket/tcp)

(define (serve port-no)
  (define listener (tcp-listen port-no 10 #t))
  (define (loop)
    (accept-and-handle listener)
    (loop))
  (define t (thread loop))
  (lambda ()
    (kill-thread t)
    (tcp-close listener)))


(define (accept-and-handle listener)
  (define-values (in out) (tcp-accept listener))
  (thread
   (lambda ()
     (handle in out)
     (close-input-port in)
     (close-output-port out))))


(define (handle in out)
  (regexp-match #rx"(\r\n|^)\r\n" in)
  (display "HTTP/1.0 200 OK\r\n" out)
  (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
  (display "<html><title>haha</title><body>hello world</body></html>" out))


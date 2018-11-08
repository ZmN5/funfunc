#lang racket

(require racket/tcp)
(require  xml net/url)

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


;; (define (handle in out)
;;   (regexp-match #rx"(\r\n|^)\r\n" in)
;;   (display "HTTP/1.0 200 OK\r\n" out)
;;   (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
;;   (display "<html><body>hello world</body></html>" out))


(define (handle in out)
  (define req
    (regexp-match #rx"^GET (.+) HTTP/[0-9]+\\.[0-9]+"
            (read-line in)))
  (when req
    (regexp-match #rx"(\r\n|^)\r\n" in)
    (let ([xexpr (dispatch (list-ref req 1))])
      (display "HTTP/1.0 200 OK\r\n" out)
      (display "Server: k\r\nContent-Type: text/xml\r\n\r\n" out)
      (display (xexpr->string xexpr) out))))


(define (dispatch str-path)
  (define url (string->url str-path))
  (define path (map path/param-path (url-path url)))
  (define h (hash-ref dispatch-table (car path) #f))
  (if h
      (h (url-query url))
      `(html (head (title "error"))
             (body
              (font ((color "red"))
                    "unknow page:"
                    ,str-path)))))

(define dispatch-table (make-hash))


(define (build-request-page label next-url hidden)
  `(html
    (head (title "Enter a Number to Add"))
    (body ([bgcolor "white"])
          (form ([action ,next-url] [method "get"])
                ,label
                (input ([type "text"] [name "number"]
                                      [value ""]))
                (input ([type "hidden"] [name "hidden"]
                                        [value ,hidden]))
                (input ([type "submit"] [name "enter"]
                                        [value "Enter"]))))))


(define (many query)
  (build-request-page "Number of greetings:" "/reply" ""))

(define (reply query)
  (define n (string->number (cdr (assq 'number query))))
  `(html (body ,@(for/list ([i (in-range n)])
                   " hello"))))

(hash-set! dispatch-table "many" many)
(hash-set! dispatch-table "reply" reply)

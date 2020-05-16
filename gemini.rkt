#lang racket

(require openssl)

(struct response (status meta body) #:transparent)

(define (parse-header header)
  (let ([parts (string-split header)])
     (values (string->number (first parts))
             (second parts))))

; based on https://github.com/erkin/gophwr/blob/master/src/gopher.rkt
(define (make-request url)
  (let-values ([(in out) (ssl-connect/enable-break "gemini.circumlunar.space" 1965)])
    (display (string-append url "\r\n") out)
    (ssl-abandon-port out)
    (let* ([res (port->lines in)]
           [header (first res)]
           [body (rest res)])
      (let-values ([(status meta) (parse-header header)])
        (response status meta body)))))

(define (show-body body mimetype)
  (match mimetype
    ["text/gemini" (string-join body "\n")]
    [_ "Unknown mimetype"]))

(define (process-response res)
  (let* ([body (response-body res)]
         [meta (response-meta res)]
         [status (response-status res)]
         [redirect (thunk (fetch meta))]
         [show (thunk (show-body body meta))]
         [error (lambda (e) (string-append e ": " meta))])
  (match status
    [20 (show)]
    [21 (show)] ;TODO: delete session
    [30 (redirect)]
    [31 (redirect)]
    [40 (error "Temporary failure")]
    [41 (error "Server unavailable")]
    [42 (error "CGI error")]
    [43 (error "Proxy error")]
    [44 (string-join "Slow down, wait for " meta " seconds")]
    [50 (error "Permanent failure")]
    [51 (error "Not found")]
    [52 (error "Gone")]
    [53 (error "Proxy request refused")]
    [54 (error "Bad request")]
    [_ "Unknown response"])))

(define (fetch url)
  (process-response (make-request url)))

(fetch "gemini://gemini.circumlunar.space/capcom")
#lang racket/gui

(require openssl)
(require net/url)
(struct response (status meta body) #:transparent)

(define (parse-header header)
  (let ([parts (string-split header)])
     (values (string->number (first parts))
             (second parts))))

; based on https://github.com/erkin/gophwr/blob/master/src/gopher.rkt
(define (make-request url)
  (let-values ([(in out) (ssl-connect/enable-break
                          (url-host (string->url url))
                          (or (url-port (string->url url)) 1965))])
    (display (string-append url "\r\n") out)
    (ssl-abandon-port out)
    (let* ([res (port->lines in)]
           [header (first res)]
           [body (rest res)])
      (let-values ([(status meta) (parse-header header)])
        (response status meta body)))))

(define (show-body body mimetype)
  (send page-contents erase)
  (send page-contents insert (match mimetype
    ["text/gemini" (string-join body "\n")]
    [(regexp #rx"text/*") (string-join body "\n")]
    [_ "Unknown mimetype"])))

; TODO
(define (prompt meta)
  "Prompted")

(define (process-response res)
  (let* ([body (response-body res)]
         [meta (response-meta res)]
         [status (response-status res)]
         [redirect (thunk (fetch meta))]
         [show (thunk (show-body body meta))]
         [error (lambda (e) (string-append e ": " meta))])
  (match status
    [10 (prompt meta)]
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

(define (show-error message)
  (message-box "Error" message))

(define window
  (new frame%
       [label "Germinate"]
       [width 800]
       [height 800]))

(define panel
  (new vertical-panel%
       [parent window]))

(define address-bar
  (new text-field%
       [parent panel]
       [label "URL"]))

(define go-button
  (new button%
       [parent panel]
       [label "Go"]
       [callback (lambda (_ __) ((let ([raw-url (send address-bar get-value)])
                                   (if (url-host (string->url raw-url))
                                       (thunk (fetch raw-url))
                                       (thunk (show-error "Invalid URL"))))))]))

(define page-contents
  (new text%))

(define page
  (new editor-canvas%
       [parent panel]
       [editor page-contents]))

(send window show #t)
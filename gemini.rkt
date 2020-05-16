#lang racket/gui

(require openssl)
(require net/url)
(struct response (status meta body from-url) #:transparent)

(define (parse-header header)
  (let ([parts (string-split header)])
     (values (string->number (first parts))
             (second parts))))

(define (make-request-contents url query)
  (if query
      (string-append (url->string url) "?" query "\r\n")
      (string-append (url->string url) "\r\n")))

; based on https://github.com/erkin/gophwr/blob/master/src/gopher.rkt
(define (make-request url query)
  (let-values ([(in out) (ssl-connect/enable-break
                          (url-host url)
                          (or (url-port url) 1965))])
    (display (make-request-contents url query) out)
    (ssl-abandon-port out)
    (let ([header (read-line in)])
      (let-values ([(status meta) (parse-header header)])
        (response status meta in url)))))

(define (show-body body mimetype)
  (send page-contents erase)
  (match mimetype
    ["text/gemini" (send page-contents insert (string-join (port->lines body) "\n"))]
    [(regexp #rx"text/*") (send page-contents insert (string-join (port->lines body) "\n"))]
    [_ (write-bytes (port->bytes body) (open-output-file (put-file "Choose a location to save file") #:exists 'replace))]))
 

(define (prompt prompted-by meta)
  (let ([input (get-text-from-user "Input required" meta)])
    (fetch prompted-by input)))

(define (process-response res)
  (let* ([body (response-body res)]
         [from-url (response-from-url res)]
         [meta (response-meta res)]
         [status (response-status res)]
         [redirect (thunk (fetch (string->url meta)))]
         [show (thunk (show-body body meta))]
         [error (lambda (e) (string-append e ": " meta))])
  (match status
    [10 (prompt from-url meta)]
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

(define (fetch url [query #f])
  (process-response (make-request url query)))

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
       [callback (lambda (_ __) ((let ([raw-url (string->url (send address-bar get-value))])
                                   (unless (url-scheme raw-url)
                                     (set-url-scheme! raw-url "gemini"))
                                   (if (url-host raw-url)
                                       (thunk (fetch raw-url))
                                       (thunk (show-error "Invalid URL"))))))]))

(define page-contents
  (new text%))

(define page
  (new editor-canvas%
       [parent panel]
       [editor page-contents]))

(send window show #t)
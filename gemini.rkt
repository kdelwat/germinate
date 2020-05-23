#lang racket/gui

(require openssl)
(require net/url)
(struct response (status meta body from-url) #:transparent)

; History
(define history-stack '())

(define (pop-history)
  (define top (first history-stack))
  (set! history-stack (rest history-stack))
  top)

(define (push-history url)
  (set! history-stack (append (list url) history-stack)))

(define (back-enabled)
  (> (length history-stack) 1))

(define thread-custodian 'nil)

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

(define header-style
  (let ([delta (make-object style-delta% 'change-bold)])
    (send the-style-list find-or-create-style #f delta)))

(define link-style
  (let ([delta (make-object style-delta% 'change-italic)])
    (send delta set-delta-foreground "DodgerBlue")
    (send the-style-list find-or-create-style #f delta)))

(define text-style
  (send the-style-list basic-style))

(define (make-link url [text #f])
  (define link-snip%
    (class string-snip%
      (inherit get-flags set-flags)
      (super-new)
      (set-flags (cons 'handles-all-mouse-events (get-flags)))
      (define/override (on-event dc x y editorx editory e)
        (when (send e button-down? 'left)
          (fetch (string->url url))))))
  
  (define link (new link-snip%))
  (send link set-style link-style)
  
  (let ([link-text (or text url)])
    (send link insert link-text (string-length link-text)))
  
  link)

(define (make-text text)
  (define text-snip (new string-snip%))
  (send text-snip set-style text-style)
  (send text-snip insert text (string-length text))
  text-snip)

(define (line->header line)
  (define text-snip (new string-snip%))
  (send text-snip set-style header-style)
  (send text-snip insert line (string-length line))
  text-snip)

; Gemini text
(define gemini-link-re #px"=>\\s*(\\S*)\\s*(.*)")
(define gemini-header-re #px"^#{1,3}.*")

(define (line->gemini-link line base-url)
  (let* ([match (regexp-match gemini-link-re (string-trim line))]
         [link-url (second match)]
         [absolute-url (if (string-prefix? link-url "gemini://")
                           link-url
                           (string-append (url->string base-url) link-url))])
    (if (= (length match) 3)
        (make-link absolute-url (third match))
        (make-link absolute-url))))

(define (show-gemini-text lines base-url)
  (for/list ([line lines])
    (send page-contents insert
          (match line
            [(regexp gemini-link-re) (line->gemini-link line base-url)]
            [(regexp gemini-header-re) (line->header line)]
            [_ (make-text line)]))
    (send page-contents insert "\n")))

(define (show-body body mimetype base-url)
  (send page-contents erase)

  (match mimetype
    ["text/gemini" (show-gemini-text (port->lines body) base-url)]
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
         [show (thunk (show-body body meta from-url))]
         [error (lambda (e) (string-append e ": " meta))])
    (when (or (< status 30) (> status 39))
      (push-history from-url))
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
      [_ "Unknown response"])
    (send address-bar set-value (url->string from-url))
    (send status-bar set-label (string-append "Ready"))))

(define (initiate-user-fetch url [query #f])
  (when (custodian? thread-custodian)
      (custodian-shutdown-all thread-custodian))
  (fetch url query))

(define (fetch url [query #f])
  (unless (and (custodian? thread-custodian) (not (custodian-shut-down? thread-custodian)))
      (set! thread-custodian (make-custodian)))
  

  (parameterize ([current-custodian thread-custodian])
    (thread (thunk 
             (do-fetch url query)))))

(define (do-fetch url query)
  (send status-bar set-label (string-append "Loading " (url->string url) "..."))
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

(define address-bar-panel
  (new horizontal-panel%
       [parent panel]
       [stretchable-height #f]))

(define back-button
  (new button%
       [parent address-bar-panel]
       [label "Back"]
       [callback (lambda (_ __) (and (back-enabled) (initiate-user-fetch (and (pop-history) (pop-history)))))]))

(define address-bar
  (new text-field%
       [parent address-bar-panel]
       [label "URL"]))

(define go-button
  (new button%
       [parent address-bar-panel]
       [label "Go"]
       [callback (lambda (_ __) ((let ([raw-url (string->url (send address-bar get-value))])
                                   (unless (url-scheme raw-url)
                                     (set-url-scheme! raw-url "gemini"))
                                   (if (url-host raw-url)
                                       (thunk (initiate-user-fetch raw-url))
                                       (thunk (show-error "Invalid URL"))))))]))

(define page-contents
  (new text%))

(define status-bar
  (new message%
       [label "Ready"]
       [parent panel]))

(define page
  (new editor-canvas%
       [parent panel]
       [editor page-contents]))

(send window show #t)

(initiate-user-fetch (string->url "gemini://gemini.circumlunar.space"))
#lang racket/gui

; Gemini is a modern interpretation of the Gopher protocol, with a few additions
; to make client development easier.
;
; This file implements a minimal Gemini browser.
;
; It follows the specification at:
; * https://gemini.circumlunar.space/docs/spec-spec.txt
; * gemini://gemini.circumlunar.space/docs/spec-spec.txt

; Unlike Gopher, Gemini requires SSL for all connections
(require openssl)

; We're going to do some operations on URLs, so we'll need this.
; A Gemini URL looks like gemini://gemini.circumlunar.space/docs/spec-spec.txt
(require net/url)

; Before we get to Gemini itself, some basic browser things.

; History first. We'll represent history as a stack of visited URLs.
(define history-stack '())

; After successfully loading a URL, we'll add it to the top of the stack
(define (push-history url)
  (set! history-stack (append (list url) history-stack)))

; When going back, we'll first discard the top of the stack (since it's the current page),
; then take the new top as our destination
(define (pop-history)
  (define next (second history-stack))
  (set! history-stack (rest (rest history-stack)))
  next)

; To be able to pop, we need at least two items in the stack (so the back button will be
; disabled until we've visited at least one link
(define (back-enabled)
  (> (length history-stack) 1))

; On to Gemini. When making a request, the server should respond with one or two items:
;
; 1. A status header
; 2. The page body, if any

(struct response (status meta body from-url) #:transparent)

; The status header is a CRLF-terminated line of text made of a status code
; and maybe metadata, seperated by a space. The ensuing body is determined by
; this header.
;
; Like:
; * "20\r\n"
; * "20 text/gemini\r\n"
; * "30 gemini://gemini.circumlunar.space/\r\n"

(define (parse-header header)
  (let ([parts (string-split header)])
     (values (string->number (first parts))
             (second parts))))


; A request is very simple: the client should just send the URL of the requested
; resource, CLRF-terminated. Gemini supports an optional query string, which
; is used for user input.
(define (make-request-contents url query)
  (if query
      (string-append (url->string url) "?" query "\r\n")
      (string-append (url->string url) "\r\n")))

; Let's make a request now.
; (This part is based on https://github.com/erkin/gophwr/blob/master/src/gopher.rkt)
(define (make-request url query)
  ; We set up an SSL connection to the requested URL's host - this will be the Gemini server
  ; The default port for Gemini is 1965
  (let-values ([(in out) (ssl-connect/enable-break
                          (url-host url)
                          (or (url-port url) 1965))])

    ; After setting up the connection, Racket provides us with input/output ports
    ; Write the requested URL to the output port to send it to the server, then close
    ; the output port (we're finished with it).
    (display (make-request-contents url query) out)
    (ssl-abandon-port out)

    ; Read in the status header and parse it
    ; Keep the input port open - there could be a body coming, and we want to handle it
    ; differently based on the status header
    (let ([header (read-line in)])
      (let-values ([(status meta) (parse-header header)])
        (response status meta in url)))))

; Now let's handle the response
(define (process-response res)
  (let* ([body (response-body res)]
         [from-url (response-from-url res)]
         [meta (response-meta res)]
         [status (response-status res)]

         ; There's a few things we might do after receiving a response:

         ; We can redirect to another URL
         [redirect (thunk (fetch (string->url meta)))]

         ; We can show the response body
         [show (thunk (show-body body meta from-url))]

         ; We can show an error
         [error (lambda (e) (string-append e ": " meta))]

         ; Or we can prompt the user for some information, and call the same
         ; URL again with their response appended as a query string
         [prompt (thunk
             (let ([input (get-text-from-user "Input required" meta)])
               (fetch from-url input)))])

    ; When the response isn't a redirect, add the fetched URL to the browser history
    (when (or (< status 30) (> status 39))
      (push-history from-url))

    ; Update the address bar with the new URL
    (send address-bar set-value (url->string from-url))
    
    ; Finally, to determine which action to perform, we'll use the status code component of
    ; the header. The first digit of the status code groups related responses, but the second
    ; can be used to obtain some more information.
    (match status
      ; 1x codes ask for user input
      [10 (prompt)]

      ; 2x codes indicate a successful response, with body
      [20 (show)]
      [21 (show)]

      ; 3x codes ask for a redirect to a new URL
      [30 (redirect)]
      [31 (redirect)]

      ; 4x codes indicate a temporary failure
      [40 (error "Temporary failure")]
      [41 (error "Server unavailable")]
      [42 (error "CGI error")]
      [43 (error "Proxy error")]
      [44 (error (string-join "Slow down, wait for " meta " seconds"))]

      ; 5x codes indicate a permanent failure
      [50 (error "Permanent failure")]
      [51 (error "Not found")]
      [52 (error "Gone")]
      [53 (error "Proxy request refused")]
      [54 (error "Bad request")]
      
      [_ "Unknown response"])

    ; Tell the user we've processed the response and are ready to take another
    ; request
    (send status-bar set-label (string-append "Ready"))))

; We can now make requests and handle responses, so let's tie it all together
; We only want the user to make a single request at a time, but if it's taking too
; long, or they change their mind, they should be able to replace it with a new
; request.
; Racket provides a helpful custodian feature which will manage resources created
; underneath it; that way, cancelling a request will cancel all resources it uses

(define thread-custodian 'nil)

; When a user clicks a link, or otherwise tries to make a request, shut down the
; current custodian (ending any ongoing request), then make the request
(define (initiate-user-fetch url [query #f])
  (when (custodian? thread-custodian)
    (custodian-shutdown-all thread-custodian))
  (fetch url query))

; To make the request, create a new custodian to manage its resources
(define (fetch url [query #f])
  (unless (and (custodian? thread-custodian) (not (custodian-shut-down? thread-custodian)))
      (set! thread-custodian (make-custodian)))
  
  (parameterize ([current-custodian thread-custodian])
    ; After using parameterize, any resources created will be bound to the new request's custodian
    ; Since we don't want a long-running request to block user input, which would make for a frozen
    ; interface, we'll spin it off into its own thread.
    (thread (thunk 
             (do-fetch url query)))))

(define (do-fetch url query)
  ; When we make the request, inform the user we're loading the URL
  (send status-bar set-label (string-append "Loading " (url->string url) "..."))

  ; Then call the server, and process the response. Easy!
  (process-response (make-request url query)))

; That's the meat-and-potatoes sorted. Now it's just a matter of setting up the user interface.

; We create a window to hold the GUI.
(define window
  (new frame%
       [label "Germinate"]
       [width 800]
       [height 800]))

; Elements will be laid out vertically, top-to-bottom
(define panel
  (new vertical-panel%
       [parent window]))

; At the top of the screen is an address bar, with some helpful buttons
(define address-bar-panel
  (new horizontal-panel%
       [parent panel]
       [stretchable-height #f]))

; The back button will return to the previous page, if there is one
(define back-button
  (new button%
       [parent address-bar-panel]
       [label "Back"]
       [callback (lambda (_ __) (and (back-enabled) (initiate-user-fetch (pop-history))))]))

; The address bar itself is a text field for the user to enter a URL
(define address-bar
  (new text-field%
       [parent address-bar-panel]
       [label "URL"]))

; The "go" button makes a request to the URL in the address bar
(define go-button
  (new button%
       [parent address-bar-panel]
       [label "Go"]
       [callback (lambda (_ __) ((let ([raw-url (string->url (send address-bar get-value))])
                                   ; If the user didn't specify the URL scheme (with "gemini://"), default to Gemini
                                   (unless (url-scheme raw-url)
                                     (set-url-scheme! raw-url "gemini"))

                                   ; If the URL has a valid host, make the request
                                   ; Otherwise, warn the user
                                   (if (url-host raw-url)
                                       (thunk (initiate-user-fetch raw-url))
                                       (thunk (show-error "Invalid URL"))))))]))

; The status bar can be used to show messages to the user - it helps ensure they don't think the browser
; is frozen on long-running requests
(define status-bar
  (new message%
       [label "Ready"]
       [parent panel]))

; On to the actual page being viewed. To show a page, we'll use Racket's editor class. Since we don't want
; to drag elements around the page, a simple text editor will suffice.
(define page-contents
  (new text%))

; We wrap the text editor in a canvas to display it, and add it to the main window.
(define page
  (new editor-canvas%
       [parent panel]
       [editor page-contents]))

; With that, the GUI layout is complete! Let's see how to display the pages we request.
; We'll handle bodies returned from the Gemini server in three different ways.
(define (show-body body mimetype base-url)
  ; No matter the contents, we'll clear the current page first
  (send page-contents erase)

  ; Then, we check the mimetype (or file type) that the server has returned
  (match mimetype
    ; Gemini has a special file format with some handy features like links, so we handle
    ; that specially
    ["text/gemini" (show-gemini-text (port->lines body) base-url)]

    ; For any other text-based format, we show the text directly in the GUI
    [(regexp #rx"text/*") (send page-contents insert (string-join (port->lines body) "\n"))]

    ; If the body isn't text, we'll let the user save it as a binary file to their computer
    [_ (write-bytes (port->bytes body) (open-output-file (put-file "Choose a location to save file") #:exists 'replace))]))

; The Gemini text format is really simple. All we're going to handle are links, headers, and text lines.
; To show content in the Racket editor, we use "snips", which wrap a group of characters with styles and event handlers.

; Links take the form "=>[<whitespace>]<URL>[<whitespace><USER-FRIENDLY LINK NAME>]<CR><LF>"
(define gemini-link-re #px"=>\\s*(\\S*)\\s*(.*)")

; Because links have a click behaviour, they're the trickiest to implement.
(define (make-link url [text #f])
  ; To create a link, we'll define a new snip and override its click event - so when the user clicks, we
  ; go to the link URL.
  (define link-snip%
    (class string-snip%
      (inherit get-flags set-flags)
      (super-new)
      (set-flags (cons 'handles-all-mouse-events (get-flags)))
      (define/override (on-event dc x y editorx editory e)
        (when (send e button-down? 'left)
          (fetch (string->url url))))))

  (define link (new link-snip%))
  
  ; Then we style it - blue and italic
  (define link-style
    (let ([delta (make-object style-delta% 'change-italic)])
      (send delta set-delta-foreground "DodgerBlue")
      (send the-style-list find-or-create-style #f delta)))

  (send link set-style link-style)

  ; And either show the optional user-friendly link name, or the URL itself if there is none
  (let ([link-text (or text url)])
    (send link insert link-text (string-length link-text)))
  
  link)

; Finally, we'll try to extract both the link URL and human-friendly text, then create the
; link snip.
(define (line->gemini-link line base-url)
  (let* ([match (regexp-match gemini-link-re (string-trim line))]
         [link-url (second match)]
         ; If the URL is relative, like "docs/", add it to the page's current URL to get
         ; an absolute URL
         [absolute-url (if (string-prefix? link-url "gemini://")
                           link-url
                           (string-append (url->string base-url) link-url))])
    (if (= (length match) 3)
        (make-link absolute-url (third match))
        (make-link absolute-url))))

; Headers start with "#", "##", or "###".
(define gemini-header-re #px"^#{1,3}.*")

; We'll just display them as bold text
(define (line->header line)  
  (define header-style
    (let ([delta (make-object style-delta% 'change-bold)])
      (send the-style-list find-or-create-style #f delta)))

  (define text-snip (new string-snip%))
  (send text-snip set-style header-style)
  (send text-snip insert line (string-length line))
  text-snip)

; Plain text lines are anything else
(define (line->text text)
  (define text-snip (new string-snip%))
  (send text-snip set-style (send the-style-list basic-style))
  (send text-snip insert text (string-length text))
  text-snip)

; Now that we know how to display each type of Gemini text line,
; loop through the lines and do it!
(define (show-gemini-text lines base-url)
  (for/list ([line lines])
    (send page-contents insert
          (match line
            [(regexp gemini-link-re) (line->gemini-link line base-url)]
            [(regexp gemini-header-re) (line->header line)]
            [_ (line->text line)]))
    (send page-contents insert "\n")))

; If at any point an error occurs, we can show it to the user in a pop-up
(define (show-error message)
  (message-box "Error" message))

; And that's all! We've built the request-response cycle and the GUI.
; It's time to start up the GUI...
(send window show #t)
; And request the homepage!
(initiate-user-fetch (string->url "gemini://gemini.circumlunar.space"))
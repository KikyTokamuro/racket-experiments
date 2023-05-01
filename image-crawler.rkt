#lang racket/gui

(require racket/class
         net/url
         net/url-string
         sxml/sxpath
         html-parsing
         racket/draw)

(define images-list '())

(define (page->xexp url)
  (call/input-url (string->url url)
                  get-pure-port
                  html->xexp))

(define (normalize-file-url base-url file-url)
  (url->string (combine-url/relative (string->url base-url) file-url)))

(define (images-list-from-url url)
  (define page-xexp (page->xexp url))
  (define images-xexp ((sxpath "//img/@src") page-xexp))
  (map
   (lambda (img) 
     (normalize-file-url url (cadr img)))
   images-xexp))

(define (bitmap-from-url url)
  (let* ((url url)
         (port (get-pure-port url))
         (img (read-bitmap port)))
    img))

(define (search-btn-handler btn e)
  (send images-box clear)
  (define url (send url-field get-value))
  (for ([image (images-list-from-url url)])
    (send images-box append image)))

(define (images-box-handler ib e)
  (define selection (send ib get-string-selection))
  (define image (bitmap-from-url (string->url selection)))
  (define dc (send image-viewer get-dc))
  (send dc clear)
  (send dc draw-bitmap image 0 0))

(define window
  (new frame%
       [label "Image crawler"]
       [width 500]
       [height 500]))

(define url-field
  (new text-field%
       [parent window]
       [label "Url: "]))

(define search-btn
  (new button%
       [parent window]
       [label "Search"]
       [stretchable-width 1]
       [callback search-btn-handler]))

(define result-panel
  (new group-box-panel%
       [parent window]
       [label "Result"]))

(define images-box
  (new list-box%
       [parent result-panel]
       [choices images-list]
       [label #f]
       [min-height 200]
       [callback images-box-handler]))

(define image-viewer
  (new canvas%
       [parent result-panel]
       [min-height 400]))

(send window show #t)

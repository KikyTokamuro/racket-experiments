#lang racket/base

(require racket/gui/easy
         racket/gui/easy/operator
         racket/class
         racket/format
         racket/draw
         racket/string
         net/url
         simple-http)

(define reddit-com (update-ssl (update-host json-requester "www.reddit.com") #t))

(define/obs @urls '())

(define/obs @current-sub "")
(define/obs @current-image (make-object bitmap% 1 1))
(define/obs @scale 2)

(define (bitmap-from-url url)
  (let* ([port (get-pure-port url)] [img (read-bitmap port)]) img))

(define (parse-subreddit sub)
  (define resp (json-response-body (get reddit-com (string-append "/r/" sub "/top.json"))))
  (define posts
    (foldl (λ (child lst)
             (let* ([post-data (hash-ref child 'data)]
                    [title (hash-ref post-data 'title)]
                    [img-url (hash-ref post-data 'url_overridden_by_dest)])
               (if (string-contains? img-url "gif") lst (cons (cons title img-url) lst))))
           '()
           (hash-ref (hash-ref resp 'data) 'children)))
  posts)

(define (draw-image dc image-data)
  (let ([bitmap (car image-data)] [scale (cadr image-data)])
    (begin
      (send dc clear)
      (send dc draw-bitmap bitmap 0 0)
      (send dc set-scale (/ scale 100) (/ scale 100)))))

(define (make-input-panel)
  (vpanel #:stretch '(#t #f)
          (hpanel (input @current-sub #:label "Subreddit" (λ (_event text) (:= @current-sub text)))
                  (button "View" (λ () (:= @urls (parse-subreddit (obs-peek @current-sub))))))
          (slider @scale #:label "Zoom" #:style '(horizontal plain) (λ:= @scale))))

(define (make-preview-panel)
  (group "Image"
         #:margin '(5 0)
         #:alignment '(left top)
         (canvas (obs-combine (λ (i s) (list i s)) @current-image @scale) draw-image)))

(define (make-posts-panel)
  (group "Posts"
         #:margin '(5 0)
         #:stretch '(#f #t)
         #:min-size '(210 #f)
         #:alignment '(left top)
         (list-view
          @urls
          #:key car
          (λ (label k)
            (button (~a label #:max-width 25 #:limit-marker "...")
                    #:min-size '(180 #f)
                    (λ () (:= @current-image (bitmap-from-url (string->url (cdr (obs-peek k)))))))))))

(define app
  (window #:title "Reddit image viewer"
          #:size '(800 700)
          #:stretch '(#t #t)
          (vpanel (make-input-panel)
                  (hpanel #:alignment '(left top) (make-posts-panel) (make-preview-panel)))))

(module+ main
  (render app))

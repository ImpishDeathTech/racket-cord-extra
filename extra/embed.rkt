#lang racket
(require racket/date
         "looping.rkt")

(define (jsexpr-list? expr)
  (let ([out #true])
    (if (list? expr)
        (sigma ([v expr] (null? v) (cdr v))
               (unless (hash? (car v))
                 (set! out #false)
                 (set! v '(null))))
        (set! out #false))
    out))

(define embed-builder%
  (class object%
    (super-new)
    (init-field [title "‎ "]
                [description "‎ "]
                [color 0])
    (field [embed-expr (make-hasheq)]
           [embed-fields null]
           [timestamp #f])

    ; add multiple fields at once as raw hashes
    (define/public (add-fields . arg-list)
      (if (jsexpr-list? arg-list)
          (set-field! embed-fields this (append (get-field embed-fields this) arg-list))
          (raise-argument-error 'add-fields "(listof/c jsexpr?)" arg-list)))

    ; add a single feild to the embed
    (define/public (add-feild name description #:inline? [inline? #false])
      (if inline?
          (set-field! embed-fields this (append (get-field embed-feilds this) (hasheq 'name name 'description description 'inline inline?)))
          (set-field! embed-fields this (append (get-field embed-feilds this) (hasheq 'name name 'description description)))))

    ; clear all of the embed fields
    (define/public (clear-fields)
      (set-field! embed-fields this null))

    ; set the title url
    (define/public (set-url new-url)
      (if (string? new-url)
          (hash-set! (get-field embed-expr this) 'url new-url)
          (raise-argument-error 'set-url "string?" new-url)))

    ; set the embed title
    (define/public (set-title new-title)
      (if (string? new-title)
          (set-field! title this new-title)
          (raise-argument-error 'set-title "string?" new-title)))

    ; set the embed description
    (define/public (set-description new-description)
      (if (string? new-description)
          (set-field! description this new-description)
          (raise-argument-error 'set-description "string?" new-description)))

    (define/public (set-color color)
      (if (exact-nonnegative-integer? color)
          (set-field! color this color)
          (raise-argument-error 'set-color "exact-nonnegative-integer?" color)))

    ; set the authors name, icon, and optionally url
    (define/public (set-author name icon-url [url #f])
      (cond [(not (string? name))
             (raise-argument-error 'set-author "string?" name)]
            [(not (string? icon-url))
             (raise-argument-error 'set-author "string?" icon-url)]
            [else (if (not url)
                      (hash-set! (get-field embed-expr this)
                                 'author (hasheq 'name name
                                                 'icon_url icon-url))
                      (if (string? url)
                          (hash-set! (get-field embed-expr this)
                                     'author (hasheq 'name name
                                                     'icon_url icon-url
                                                     'url url))
                          (raise-argument-error 'set-author "(or/c string? #false)" url)))]))

    ; set the thumbnail url
    (define/public (set-thumbnail thumbnail-url)
      (hash-set! (get-field embed-expr this)
                 'thumbnail (hasheq 'url thumbnail-url)))

    ; set the image url
    (define/public (set-image image-url)
      (hash-set! (get-field embed-expr this)
                 'image (hasheq 'url image-url)))

    ; set the footer text and icon url
    (define/public (set-footer text icon-url)
      (hash-set! (get-field embed-expr this)
                 'footer (hasheq 'text text
                                 'icon_url icon-url)))
      
    ; set the embed's timestamp
    (define/public (set-timestamp)
      (let ([last (date-display-format)])
        (date-display-format 'iso-8601)
        (hash-set! (get-field embed-expr this) (date->string (current-date) #true))
        (date-display-format last)))

    ; build the embed
    (define/public (build)
      (let ([output (get-field embed-expr this)])
        (hash-set*! output
                    'title (get-field title this)
                    'description (get-field description this)
                    'color (get-field color this)
                    'fields (get-field embed-fields this))
        output))
                    

    (define/public (get-jsexpr)
      (send this build))))

(define (make-embed [title "‎ "] [description "‎ "] [color 0])
  (cond [(not (string? title)) (raise-argument-error 'make-embed "string?" title)]
        [(not (string? description)) (raise-argument-error 'make-embed "string?" description)]
        [(not (exact-nonnegative-integer? color)) (raise-argument-error 'make-embed "exact-nonnegative-integer?" color)]
        [else (make-object embed-builder% title description color)]))

(define (new-embed)
  (new embed-builder%))

(define (embed-builder? builder)
  (is-a? builder embed-builder%))

(provide (all-defined-out))
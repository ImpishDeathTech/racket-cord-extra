#lang racket
(require (only-in net/base64
                  base64-encode)
         racket/generic
         "looping.rkt")

(define PNG "image/png")
(define JPEG "image/jpeg")
(define GIF "image/gif")
(define WebP "image/webp")

(define (image-type path)
  (let ([type (string-split path ".")])
    (set! type (string->symbol (list-ref type (sub1 (length type)))))
    (case type
      [(png) 'PNG]
      [(gif) 'GIF]
      [(jpg jpeg) 'JPEG]
      [(webp) 'WebP])))

(define (load-image file-path)
  (cond [(path? file-path) (set! file-path (path->string file-path))]
        [(not (path-string? file-path)) (raise-argument-error 'load-image "(or/c path-string? path?)" file-path)])
  (with-input-from-file file-path #:mode 'binary
    (thunk (base64-encode (read-bytes (file-size file-path))))))

(provide (prefix-out http: (combine-out image-type
                                        load-image))
         PNG
         JPEG
         GIF
         WebP)
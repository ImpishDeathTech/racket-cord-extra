#lang racket

(require "image-data.rkt")

(define-struct attachment (data type name)
  #:constructor-name attachment
  #:transparent)

(provide (contract-out
          [struct attachment
            ([data bytes?]
             [type (or/c bytes? string?)]
             [name (or/c bytes? string?)])]))

(define attachment-builder%
  (class object%
    (super-new)
    (init-field [file-path ""])
    
    (define/public (set-path path)
      (cond [(path-string? path) (set-field! file-path this path)]
            [(path? path) (set-field! file-path this (path->string path))]
            [else (raise-argument-error 'set-path "(or/c path-string? path?)" path)]))

    (define/public (get-name)
      (let ([ls null])
        (case (system-type)
          [(unix macosx)
           (set! ls (string-split (get-field file-path this) "/"))]
          [(windows)
           (set! ls (string-split (get-field file-path this) "\\"))])
        (if (> (length ls) 0) 
            (list-ref ls (sub1 (length ls)))
            #f)))
                      
    
    (define/public (get-path)
      (string->path (get-field file-path this)))

    (define/public (get-type)
      (http:image-type (send this get-path)))
    
    (define/public (get-url)
      (string-append "attachment://" (send this get-name)))
    
    (define/public (build)
      (attachment (http:load-image (get-field file-path this))
                  (format "image/~a" (string-downcase (symbol->string (send this get-type))))
                  (send this get-name)))))

(define (attachment-builder? attachment)
  (is-a? attachment attachment-builder%))

(define (make-attachment [file-path #false])
  (cond [(not file-path) (new attachment-builder%)]
        [(path-string? file-path) (make-object attachment-builder% file-path)]
        [(path? file-path) (make-object attachment-builder% (path->string file-path))]
        [else (raise-argument-error 'make-attachment "(or/c path-string? path?)" file-path)]))

(provide attachment-builder%
         attachment-builder?
         make-attachment)

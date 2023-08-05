#lang racket/base
(require racket/generic
         racket/contract
         json
         "looping.rkt")

(define-generics sticker-methods
  (sticker-asset sticker-methods)
  (sticker-available? sticker-methods)
  (sticker-description sticker-methods)
  (sticker-format-type sticker-methods)
  (sticker-guild-id sticker-methods)
  (sticker-id sticker-methods)
  (sticker-name sticker-methods)
  (sticker-tags sticker-methods)
  (sticker-type sticker-methods)
  (sticker-url sticker-methods))

(provide
 sticker-asset
 sticker-available?
 sticker-description
 sticker-format-type
 sticker-guild-id
 sticker-id
 sticker-name
 sticker-tags
 sticker-type
 sticker-url)

(define-struct sticker
  (data)
  #:methods gen:sticker-methods
  [(define (sticker-asset s)
     (hash-ref (sticker-data s) 'asset))

   (define (sticker-available? s)
     (hash-ref (sticker-data s) 'available))

   (define (sticker-description s)
     (hash-ref (sticker-data s) 'description))

   (define (sticker-format-type s)
     (hash-ref (sticker-data s) 'format_type))

   (define (sticker-guild-id s)
     (hash-ref (sticker-data s) 'guild_id))

   (define (sticker-id s)
     (hash-ref (sticker-data s) 'id))

   (define (sticker-name s)
     (hash-ref (sticker-data s) 'name))

   (define (sticker-tags s)
     (hash-ref (sticker-data s) 'tags))

   (define (sticker-type s)
     (hash-ref (sticker-data s) 'type))

   (define (sticker-url sticker)
     (format "http://cdn.discordapp/stickers/~a.png" (sticker-id sticker)))])


(define (listof-jsexpr->listof-stickers jslist)
  (define output null)
  (sigma ([ls jslist] (null? ls) (cdr ls))
         (if (jsexpr? (car ls))
             (set! output (cons (make-sticker (car ls)) output))
             (raise-argument-error 'listof-jsexpr->listof-stickers "(listof/c jsexpr?)" ls)))
  (reverse output))

(define (listof-stickers->listof-jsexpr stickerlist)
  (define output null)
  (sigma ([ls stickerlist] (null? ls) (cdr ls))
         (if (sticker? (car ls))
             (set! output (cons (sticker-data (car ls)) output))
             (raise-argument-error 'listof-stickers->listof-jsexpr "(listof/c sticker?)" ls)))
  (reverse output))

(provide (contract-out
          [struct sticker
            ([data jsexpr?])]))
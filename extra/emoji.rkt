#lang racket/base
(require racket/generic
         racket/contract
         json)

(define-generics emoji-methods
  (emoji-animated? emoji-methods)
  (emoji-available? emoji-methods)
  (emoji-id emoji-methods)
  (emoji-managed? emoji-methods)
  (emoji-name emoji-methods)
  (emoji-require-colons? emoji-methods)
  (emoji-roles emoji-methods)
  (emoji-url emoji-methods)
  (emoji->string emoji-methods))

(define-struct emoji
  (data)
  #:methods gen:emoji-methods
  [(define (emoji-animated? e)
     (hash-ref (emoji-data e) 'animated))

   (define (emoji-available? e)
     (hash-ref (emoji-data e) 'available))

   (define (emoji-id e)
     (hash-ref (emoji-data e) 'id))

   (define (emoji-managed? e)
     (hash-ref (emoji-data e) 'managed))

   (define (emoji-require-colons? e)
     (hash-ref (emoji-data e) 'require_colons))

   (define (emoji-name e)
     (hash-ref (emoji-data e) 'name))

   (define (emoji-roles e)
     (hash-ref (emoji-data e) 'roles))

   (define (emoji-url emoji)
     (if (emoji-animated? emoji)
         (format "http://cdn.discordapp.com/emojis/a_~a.gif" (emoji-id emoji))
         (format "http://cdn.discordapp.com/emojis/~a.webp" (emoji-id emoji))))

   (define (emoji->string emoji)
     (if (emoji-animated? emoji)
         (format "<a:~a:~a>" (emoji-name emoji) (emoji-id emoji))
         (format "<:~a:~a>" (emoji-name emoji) (emoji-id emoji))))])

(provide (contract-out
          [struct emoji
            ([data jsexpr?])])
         emoji-animated?
         emoji-available?
         emoji-id
         emoji-managed?
         emoji-name
         emoji-require-colons?
         emoji-roles
         emoji-url
         emoji->string)
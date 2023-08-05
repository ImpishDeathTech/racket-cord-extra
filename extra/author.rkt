#lang racket/base
(require racket/generic
         racket/contract
         json
         "user.rkt")

(define-generics author-methods
  (author-avatar author-methods)
  (author-avatar-decoration author-methods)
  (author-avatar-url author-methods)
  (author-global-name author-methods)
  (author-id author-methods)
  (author-public-flags author-methods)
  (author-username author-methods)
  (author->string author-methods))
  

(define-struct author
  (data)
  #:methods gen:author-methods
  [(define (author-avatar a)
     (hash-ref (author-data a) 'avatar))

   (define (author-avatar-decoration a)
     (if (hash-has-key? (author-data a) 'avatar_decoration)
         (hash-ref (author-data a) 'avatar_decoration)
         #f))

   (define (author-id a)
     (hash-ref (author-data a) 'id))
   
   (define (author-avatar-url a)
     (format "https://cdn.discordapp.com/avatars/~a/~a.webp" (author-id a) (author-avatar a)))

   (define (author-global-name a)
     (if (hash-has-key? (author-data a) 'global-name)
         (hash-ref (author-data a) 'global_name)
         #f))

   (define (author-public-flags a)
     (if (hash-has-key? (author-data a) 'public_flags)
         (hash-ref (author-data a) 'public_flags)
         #f))

   (define (author-username a)
     (hash-ref (author-data a) 'username))

   (define (author->string a)
     (format "<@~a>" (author-id a)))])

(provide (contract-out
          [struct author
            ([data jsexpr?])])
         author-avatar
         author-avatar-decoration
         author-avatar-url
         author-global-name
         author-id
         author-public-flags
         author-username
         author->string)
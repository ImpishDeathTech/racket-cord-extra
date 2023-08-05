#lang racket/base

(require racket/generic
         racket/contract
         json)

(define-generics user-methods
  (user-avatar user-methods)
  (user-bot? user-methods)
  (user-global-name user-methods)
  (user-id user-methods)
  (user-mfa-enabled? user-methods)
  (user-name user-methods)
  (user-verified? user-methods)
  (set-user-verified! user-methods flag)
  (user->string user-methods)
  (user-avatar-url user-methods))

(provide
 user-avatar
 user-bot?
 user-global-name
 user-id
 user-mfa-enabled?
 user-name
 user-verified?
 user->string
 user-avatar-url)

(define-struct user
  ([data #:mutable])
  #:methods gen:user-methods
  [(define (user-avatar u)
     (if (hash-has-key? (user-data u) 'avatar)
         (hash-ref (user-data u) 'avatar)
         "0"))

   (define (user-bot? u)
     (if (hash-has-key? (user-data u) 'bot)
         (hash-ref (user-data u) 'bot)
         #f))

   (define (user-global-name u)
     (if (hash-has-key? (user-data u) 'global_name)
         (hash-ref (user-data u) 'global_name)
         (user-name u)))

   (define (user-id u)
     (hash-ref (user-data u) 'id))

   (define (user-mfa-enabled? u)
     (if (hash-has-key? (user-data u) 'mfa_enabled)
         (hash-ref (user-data u) 'mfa_enabled)
         #f))

   (define (user-name u)
     (hash-ref (user-data u) 'username))

   (define (user-verified? u)
     (if (hash-has-key? (user-data u) 'verified)
         (hash-ref (user-data u) 'verified)
         #f))

   (define (set-user-verified! u flag)
     (cond [(not (boolean? flag)) (raise-argument-error 'set-user-verified! "boolean?" flag)]
           [(hash-has-key? (user-data u) 'verified) (set-user-data! u (hash-set (user-data u) 'verified flag))]))

   (define (user->string user)
     (format "<@~a>" (user-id user)))

   (define (user-avatar-url user)
     (format "http://cdn.discordapp.com/avatars/~a/~a.webp" (user-id user) (user-avatar user)))])

(provide (contract-out
          [struct user
             ([data jsexpr?])]))
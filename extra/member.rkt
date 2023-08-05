#lang racket/base
(require racket/generic
         racket/contract
         json
         "author.rkt")

(define-generics member-methods
  (member-avatar member-methods)
  (member-communication-disabled-until member-methods)
  (member-deaf? member-methods)
  (member-flags member-methods)
  (member-joined-at member-methods)
  (member-muted? member-methods)
  (member-nickname member-methods)
  (member-pending? member-methods)
  (member-premium-since member-methods)
  (member-roles member-methods))

(define-struct member
  (data)
  #:methods gen:member-methods
  [(define (member-avatar m)
     (hash-ref (member-data m) 'avatar))

   (define (member-communication-disabled-until m)
     (hash-ref (member-data m) 'communication_disabled_until))

   (define (member-deaf? m)
     (hash-ref (member-data m) 'deaf))

   (define (member-flags m)
     (hash-ref (member-data m) 'flags))

   (define (member-joined-at m)
     (hash-ref (member-data m) 'joined_at))

   (define (member-muted? m)
     (hash-ref (member-data m) 'mute))

   (define (member-nickname m)
     (hash-ref (member-data m) 'nick))

   (define (member-pending? m)
     (hash-ref (member-data m) 'pending))

   (define (member-premium-since m)
     (hash-ref (member-data m) 'premium_since))

   (define (member-roles m)
     (hash-ref (member-data m) 'roles))])

(provide (contract-out
          [struct member
            ([data jsexpr?])])
         member-avatar
         member-communication-disabled-until
         member-deaf?
         member-flags
         member-joined-at
         member-muted?
         member-nickname
         member-pending?
         member-premium-since
         member-roles)
#lang racket/base
(require racket/generic
         racket/contract
         json)

(define-generics role-methods
  (role-color role-methods)
  (role-description role-methods)
  (role-flags role-methods)
  (role-hoist role-methods)
  (role-icon role-methods)
  (role-id role-methods)
  (role-managed? role-methods)
  (role-mentionable? role-methods)
  (role-name role-methods)
  (role-permissions role-methods)
  (role-position role-methods)
  (role-unicode-emoji role-methods)
  (role-icon-url role-methods)
  (role->string role-methods))

(provide
 role-color
 role-description
 role-flags
 role-hoist
 role-icon
 role-id
 role-managed?
 role-mentionable?
 role-name
 role-permissions
 role-position
 role-icon-url
 role->string)

(define-struct role
  (data)
  #:methods gen:role-methods
  [(define (role-color role)
     (hash-ref (role-data role) 'color))
   
   (define (role-description role)
     (hash-ref (role-data role) 'description))

   (define (role-flags role)
     (hash-ref (role-data role) 'flags))

   (define (role-hoist role)
     (hash-ref (role-data role) 'hoist))

   (define (role-icon role)
     (hash-ref (role-data role) 'icon))

   (define (role-id role)
     (hash-ref (role-data role) 'id))

   (define (role-managed? role)
     (hash-ref (role-data role) 'managed))

   (define (role-mentionable? role)
     (hash-ref (role-data role) 'mentionable))

   (define (role-name role)
     (hash-ref (role-data role) 'name))

   (define (role-permissions role)
     (hash-ref (role-data role) 'permissions))

   (define (role-position role)
     (hash-ref (role-data role) 'position))

   (define (role-unicode-emoji role)
     (hash-ref (role-data role) 'unicode_emoji))

   (define (role-icon-url role)
     (format "http://cdn.discordapp.com/role-icons/~a/~a.webp" (role-id role) (role-icon role)))
   
   (define (role->string role)
     (format "<@&~a>" (role-id role)))])

(provide (contract-out
          [struct role
            ([data jsexpr?])]))
   
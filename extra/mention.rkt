#lang racket/base
(require racket/generic
         racket/contract
         json
         "member.rkt"
         "looping.rkt")

(define-generics mention-methods
  (mention-avatar mention-methods)
  (mention-avatar-decoration mention-methods)
  (mention-global-name mention-methods)
  (mention-id mention-methods)
  (mention-avatar-url mention-methods)
  (mention-public-flags mention-methods)
  (mention-username mention-methods)
  (mention-member mention-methods)
  (mention->string mention-methods))

(provide
 mention-avatar
 mention-avatar-decoration
 mention-global-name
 mention-id
 mention-avatar-url
 mention-public-flags
 mention-username
 mention-member
 mention->string)

(define-struct mention
  (data)
  #:methods gen:mention-methods
  [(define (mention-avatar m)
     (hash-ref (mention-data m) 'avatar))

   (define (mention-avatar-decoration m)
     (if (hash-has-key? (member-data m) 'avatar_decoration)
         (hash-ref (mention-data m) 'avatar_decoration)
         #f))

   (define (mention-global-name m)
     (if (hash-has-key? (member-data m) 'global_name)
         (hash-ref (member-data m) 'global_name)
         (mention-username m)))

   (define (mention-id m)
     (hash-ref (mention-data m) 'id))

   (define (mention-avatar-url m)
     (format "http://cdn.discordapp.com/avatars/~a/~a.webp" (mention-id m) (mention-avatar m)))

   (define (mention-public-flags m)
     (hash-ref (mention-data m) 'public_flags))

   (define (mention-username m)
     (hash-ref (mention-data m) 'username))

   (define (mention-member m)
     (make-member (hash-ref (mention-data m) 'member)))

   (define (mention->string m)
     (format "<@~a>" (mention-id m)))])

(define (listof-jsexpr->listof-mentions hash-list)
  (define output null)
  (unless (null? hash-list)
    (sigma ([ls hash-list] (null? ls) (cdr ls))
           (if (jsexpr? (car ls))
               (set! output (cons (make-mention (car ls)) output))
               (raise-argument-error 'listof-jsexpr->listof-mentions "(listof/c jsexpr?)" ls))))
  (reverse output))

(define (listof-mentions->listof-jsexpr mentionlist)
  (define output null)
  (sigma ([ls mentionlist] (null? ls) (cdr ls))
         (if (mention? (car ls))
             (set! output (cons (mention-data (car ls)) output))
             (raise-argument-error 'listof-mentions->listof-jsexpr "(listof/c mention?)" ls)))
  (reverse output))

(provide (contract-out
          [struct mention
            ([data jsexpr?])])
         listof-jsexpr->listof-mentions
         listof-mentions->listof-jsexpr)
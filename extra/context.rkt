#lang racket
(require racket/generic
         racket/contract
         racket/sandbox
         racket/date
         json
         "member.rkt"
         "author.rkt"
         "mention.rkt"
         "user.rkt"
         "guild.rkt"
         "looping.rkt"
         (prefix-in http: "../http.rkt")
         "../private/data.rkt"
         "../private/logger.rkt")

(define-syntax-rule (return stx ...)
  (values stx ...))

(define-generics codeblock-methods
  (codeblock->sandbox codeblock-methods #:requires requires* #:allow-for-require allow-for-require*)
  (codeblock->string codeblock-methods))

(define-struct codeblock (type text)
  #:mutable
  #:transparent
  #:methods gen:codeblock-methods
   ; format a code block for discord
  [(define (codeblock->string codeblock)
     (format "```~a\n~a\n```"
             (codeblock-type codeblock)
             (codeblock-text codeblock)))
   (define (codeblock->sandbox codeblock
                              #:requires [requires* null]
                              #:allow-for-require [allow-for-require* null])
     (case (codeblock-type codeblock)
       ((scheme racket)
        (let ([code-eval (make-evaluator (codeblock-type codeblock)
                                    #:requires requires*
                                    #:allow-for-require allow-for-require*
                                  '(begin
                                     (current-output-port (open-output-string ""))
                                     (current-input-port (open-input-string ""))
                                     (current-error-port (open-output-string ""))))])
          (with-input-from-string (codeblock-text codeblock)
            (thunk
             (sigma ([v (read)] (eof-object? v) (read))
                    (code-eval v))))
          code-eval))
       (else #f)))])
                

(provide (contract-out
          [struct codeblock
            ([type symbol?]
             [text string?])])
         codeblock->sandbox
         codeblock->string)

; generic methods to facilitate interraction with
; command context
(define-generics context-methods
  (get-flags context-methods)
  (get-edited-timestamp context-methods)
  (get-content context-methods)
  (get-command context-methods)
  (get-embeds context-methods)
  (get-guild-id context-methods)
  (get-payload-id context-methods)
  (get-attachments context-methods)
  (get-channel-id context-methods)
  (get-nonce context-methods)
  (get-timestamp context-methods)
  (get-tts context-methods)
  (get-pinned context-methods)
  (get-referenced-message context-methods)
  (get-payload-type context-methods)
  (get-arg index context-methods)
  (get-client-user context-methods)
  (get-session-id context-methods)
  (get-author-user context-methods)
  (get-prefix context-methods)
  (get-guild context-methods id)
  (get-guild-member context-methods id)
  (get-user context-methods)
  (trigger-typing context-methods channel-id))

(provide get-flags
         get-referenced-message
         get-arg
         get-edited-timestamp
         get-content
         get-command
         get-prefix
         get-user
         get-embeds
         get-guild-id
         get-channel-id
         get-nonce
         get-timestamp
         get-tts
         get-pinned
         get-referenced-message
         get-payload-type
         get-client-user
         get-author-user
         get-guild
         get-guild-member
         get-session-id
         trigger-typing)

; parses a code block
(define (parse-code-block cls)
  (let ([output null])
    (until (string-prefix? (list->string cls) "```")
           (set! output (cons (car cls) output))
           (set! cls (cdr cls)))
    (return (list->string (reverse output)) (cdddr cls))))

; separates code blocks from content
(define (parse-code-blocks content)
  (define output-blocks null)
  (define output-content null)
  (define input null)

  (sigma ([chars (string->list content)]
          (null? chars)
          (cdr chars))
         
         (if (char-whitespace? (car chars))
             (if (string-prefix? (list->string input) "```")
                 (set! output-blocks
                       (append output-blocks
                               (list
                                (codeblock
                                 (let ([sym (string->symbol (string-trim (list->string input) "```" #:right? #false))])
                                   (if (eq? sym '||) 'text sym))
                                 (let-values ([(text ls) (parse-code-block chars)])
                                   (set! output-content (append output-content (list (car chars))))
                                   (set! chars (if (null? ls) '(null) ls))
                                   (set! input null)
                                   text)))))
                 (begin
                   (set! output-content (append output-content (append input (list (car chars)))))
                   (set! input null)))
             (set! input (append input (list (car chars)))))
         
         (when (= (length chars) 1)
           (set! output-content (append output-content input))))
  
  (return output-blocks (list->string output-content)))


; separates code blocks from context and builds arguments
(define (make-context-arguments content)
  (let-values ([(code-blocks new-content) (parse-code-blocks content)]) 
    (let ([output (with-input-from-string new-content
                                 (thunk
                                  (define output null)
                                  (sigma ([v (read)] (eof-object? v) (read))
                                         (set! output (append output (list v))))
                                  output))])
      (return (if (null? output) #() (list->vector (cdr output))) code-blocks))))



; command context container
(define-struct context (bot
                        socket
                        client
                        payload
                        author
                        member
                        mentions
                        arguments
                        blocks)
  #:constructor-name new-context
  #:mutable
  #:methods gen:context-methods
  [(define (get-flags context)
     (if (hash-has-key? (context-payload context) 'flags)
         (hash-ref (context-payload context) 'flags)
         null))

   (define (get-nonce context)
     (hash-ref (context-payload context) 'nonce))

   (define (get-tts context)
     (if (hash-has-key? (context-payload context) 'tts)
         (hash-ref (context-payload context) 'tts)
         #false))

   (define (get-user context)
     (if (hash-has-key? (context-payload context) 'user)
         (make-user (hash-ref (context-payload context) 'user))
         (make-user (hasheq))))

   (define (get-pinned context)
     (hash-ref (context-payload context) 'pinned))

   (define (get-guild-member context id)
     (if (string? id)
         (make-member (http:get-guild-member (context-client context) (get-guild-id context) id))
         (raise-argument-error 'get-guild-member "string?" id)))

   (define (get-referenced-message context)
     (hash-ref (context-payload) 'referenced_message))

   (define (get-payload-type context)
     (hash-ref (context-payload context) 'type))

   ; get the edited timestamp
   (define (get-edited-timestamp context)
     (hash-ref (context-payload context) 'edited_timestamp))

   ; get context content
   (define (get-content context)
     (if (hash-has-key? (context-payload context) 'content)
         (hash-ref (context-payload context) 'content)
         ""))

   ; get context command head
   (define (get-command context)
     (let ([content (string-split (get-content context) " ")])
       (if (null? content)
           ""
           (car content))))
           
   (define (get-prefix context)
     (if (not (context-bot context))
         ""
         (symbol->string (get-field prefix (context-bot context)))))

   ; get context embeds
   (define (get-embeds context)
     (if (hash-has-key? (context-payload context) 'embeds)
         (hash-ref (context-payload context) 'embeds)
         null))

   ; get context guild id
   (define (get-guild-id context)
     (if (hash-has-key? (context-payload context) 'guild_id)
         (hash-ref (context-payload context) 'guild_id)
         ""))

   ; get context id
   (define (get-payload-id context)
     (if (hash-has-key? (context-payload context) 'id)
         (hash-ref (context-payload context) 'id)
         ""))

   ; get context timestamp
   (define (get-timestamp context)
     (if (hash-has-key? (context-payload context) 'timestamp)
         (hash-ref (context-payload context) 'timestamp)
         (date->string (current-date) #t)))
     
   ; get the attachments
   (define (get-attachments context)
     (if (hash-has-key? (get-payload context) 'attachments)
         (hash-ref (context-payload context) 'attachments)
         null))
   
   ; get the channel id
   (define (get-channel-id context)
     (if (hash-has-key? (context-payload context) 'channel_id)
         (hash-ref (context-payload context) 'channel_id)
         ""))

   ; get the session id
   (define (get-session-id context)
     (hash-ref (context-payload context) 'session_id))
   
   (define (get-arg index context)
     (if (exact-nonnegative-integer? index)
         (vector-ref (context-arguments context) index)
         (raise-argument-error 'context-arg "exact-nonnegative-integer?" index)))

   (define (get-client-user context)
     (make-user (client-user (context-client context))))
   
   (define (trigger-typing context [channel-id #f])
     (cond [(not channel-id) (http:trigger-typing-indicator (context-client context) (get-channel-id context))]
           [(string? channel-id) (http:trigger-typing-indicator (context-client context) channel-id)]
           [else (raise-argument-error 'trigger-typing "string?" channel-id)]))])



(define (make-context socket client payload [bot #f])
  (let-values ([(arguments blocks) (if (hash-has-key? payload 'content) (make-context-arguments (hash-ref payload 'content)) (values null null))]) 
    (new-context bot socket client payload
                 (if (hash-has-key? payload 'author) (make-author (hash-ref payload 'author)) #false)
                 (if (hash-has-key? payload 'member) (make-member (hash-ref payload 'member)) #false)
                 (if (hash-has-key? payload 'mentions) (listof-jsexpr->listof-mentions (hash-ref payload 'mentions)) null)
                 arguments
                 blocks)))

(define-values (get-bot
                get-socket
                get-client
                get-payload
                get-author
                get-member
                get-mentions
                get-arguments
                get-blocks)
  (values
   context-bot
   context-socket
   context-client
   context-payload
   context-author
   context-member
   context-mentions
   context-arguments
   context-blocks))

(provide (contract-out
          [struct context
            ([bot object?]
             [socket ws-client?]
             [client client?]
             [payload jsexpr?]
             [author (or/c author? jsexpr? boolean?)]
             [member (or/c member? jsexpr? boolean?)]
             [mentions (or/c list? null?)]
             [arguments (or/c list? null?)]
             [blocks (or/c list? null?)])])
         make-context
         get-bot
         get-socket
         get-client
         get-payload
         get-author
         get-member
         get-mentions
         get-arguments
         get-blocks)

(define (date->iso-string date [time? #false])
  (let ([last-fmt (date-display-format)]
        [output #f])
    (date-display-format 'iso-8601)
    (set! output (date->string date time?))
    (date-display-format last-fmt)
    output))

(provide date->iso-string)

(define (current-date->iso-string [time? #false])
  (date->iso-string (current-date) time?))

(provide current-date->iso-string)

(define current-logging-thread
  (let ([*receiver* #f]
        [*thread* #f]
        [*path* #f])
    (lambda (#:path [path #false]
             #:level [level 'info]
             #:start? [start? #false])
      (cond [start?
             (set! *receiver* (make-log-receiver discord-logger level))
             (cond [(not path)
                    (set! *thread*
                          (thread
                           (thunk
                            (repeat
                             (let ([logv (sync *receiver*)])
                               (printf "~a - [~a] - ~a\n"
                                       (current-date->iso-string #true)
                                       (vector-ref logv 0)
                                       (vector-ref logv 1)))))))]
                   [(string? path)
                    (set! *receiver* (make-log-receiver discord-logger level))
                    (set! *path* path)
                    (set! *thread*
                          (thread
                           (thunk
                            (repeat
                             (let ([logv (sync *receiver*)])
                               (with-output-to-file *path* #:exists 'append
                                 (thunk
                                  (printf "~a - [~a] - ~a\n"
                                          (current-date->iso-string #true)
                                          (vector-ref logv 0)
                                          (vector-ref logv 1)))))))))]
                   [else (raise-argument-error 'current-logging-thread "string?" path)])]
            [(not path) *thread*]
            [(string? path) (set! *path* path)]
            [else (raise-argument-error 'current-logging-thread "string?" path)]))))

(provide current-logging-thread)

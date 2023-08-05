#lang racket
(require (only-in racket/hash hash-union)
         "../main.rkt"
         "../private/logger.rkt"
         "../private/data.rkt"
         "context.rkt"
         "looping.rkt"
         "embed.rkt"
         "author.rkt"
         "user.rkt")

(define-serializable-class listener%
  object%
  (super-new)
  (init-field [prefix 'bot.])
  (field [client #false]
         [commands (make-hasheq)]
         [developer-id ""])
    
  (define/public (setup token
                        #:developer [developer-id ""]
                        #:auto-shard [auto-shard #false]
                        #:intents [intents null]
                        #:token-type [token-type 'bot]
                        #:shard-count [shard-count 1])
    ; set developer id and build the client
    (set-field! developer-id this developer-id)
    (set-field! client this (make-client token
                                         #:auto-shard auto-shard
                                         #:intents intents
                                         #:token-type token-type
                                         #:shard-count shard-count))
    ; load events
    (on-event 'raw-user-update
              (get-field client this)
              (lambda (s c p) (send this on-user-update (make-context s c p this))))
    
    (on-event 'raw-ready
              (get-field client this)
              (lambda (s c p) (send this on-ready (make-context s c p this))))
    
    (on-event 'raw-guild-create
              (get-field client this)
              (lambda (s c p) (send this on-guild-create (make-context s c p this))))
    
    (on-event 'raw-message-create
              (get-field client this)
              (lambda (s c p) (send this on-message-create (make-context s c p this))))
    
    
    (send* this
      (add-command 'kill
                   "Shuts down the bot safely"
                   (lambda (ctx)
                     (http:trigger-typing-indicator (get-client ctx) (get-channel-id ctx))
                     ; when the author id matches the developer id, kill the bot with a message
                     (when (string=? (author-id (get-author ctx))
                                     (get-field developer-id (get-bot ctx)))
                       (update-status (get-client ctx) #:status "invisible")
                       (sleep 1)
                       (http:create-message (get-client ctx)
                                            (get-channel-id ctx)
                                            #:embed (make-embed "Shutting Down" (format "<t:~a:F>" (current-seconds))))
                       (stop-client (get-client ctx))
                       (sleep 1)
                       (log-discord-info (format "SHUTDOWN COMPLETE: ~a" (current-date->iso-string #t))))))
      
      (add-command 'help
                   "Sends a help embed"
                   (lambda (ctx)
                     (http:trigger-typing-indicator (get-client ctx) (get-channel-id ctx))
                     ; if mod key is a symbol, search plug-ins for key
                     (let ([embed (make-embed "Help Desk" "A list of this bot's commands" #x00FFFF)])
                       (sigma ([keys (hash-keys (get-field commands (get-bot ctx)))] (null? keys) (cdr keys))
                              (send embed add-fields (hasheq 'name (format "`~a~a`" (get-prefix ctx) (car keys)) 'value (hash-ref (hash-ref (get-field commands (get-bot ctx)) (car keys)) 'description))))
                       (http:create-message (get-client ctx)
                                            (get-channel-id ctx)
                                            #:embed embed))))))

  ; adds a single command to the base commands module
  (define/public (add-command sym note proc)
    (hash-set! (get-field commands this) sym (hasheq 'procedure proc
                                                     'description note)))
    
  ; dispatches a command from a module
  (define/public (on-dispatch command context)
    (when (hash-has-key? (get-field commands this) command)
        ((hash-ref (hash-ref (get-field commands this) command) 'procedure) context)))
  
  ; default ready method
  (define/public (on-ready ctx)
    (let ([session-id (get-session-id ctx)]
          [user (get-user ctx)])
      (log-discord-debug "SESSION ID: ~a" session-id)
      (set-ws-client-session-id! (get-socket ctx) session-id)
      (send this on-user-update (struct-copy context
                                             ctx
                                             [payload (user-data user)]))))
  
  ; default user update method
  (define/public (on-user-update ctx)
      (let([merged (hash-union (client-user (get-client ctx))
                               (get-payload ctx)
                               #:combine (lambda (v1 v2) v2))])
        (set-client-user! (get-client ctx) merged)))
  
  ; default guild create event, to be overidden if needed
  (define/public (on-guild-create ctx)
    (void))
  
  ; default message create method, can be overrided to fit your needs
  (define/public (on-message-create ctx)
    ;check id to see if it matches our own
    (unless (string=? (author-id (get-author ctx)) (user-id (get-client-user ctx)))
        ; if not, check the string prefix to see if it matches our own
      (when (string-prefix? (list-ref (string-split (hash-ref (context-payload ctx) 'content) " ") 0) (get-prefix ctx))
        (send this on-dispatch
              (string->symbol (string-trim (list-ref (string-split (hash-ref (context-payload ctx) 'content) " ") 0) (get-prefix ctx) #:right? #false))
              ctx))))
  
  ; preforms run protocols for bot
  (define/public (start #:level [log-level 'debug]
                        #:path [log-path #false])
    ; start current logging thread
    (current-logging-thread #:start? #true
                            #:level log-level
                            #:path log-path)
    ; start client and kill current logging thread when bot is killed 
    (start-client (get-field client this))
    ; kill the logging thread when finished, if it is still running
    (when (thread-running? (current-logging-thread))
      (kill-thread (current-logging-thread))))
  
  (define/public (start-no-wait #:level [log-level 'debug]
                                #:path [log-path #false])
    (current-logging-thread #:start? #true
                            #:level log-level
                            #:path log-path)
    (start-client-no-wait (get-field client this))))
  
(define (listener? listener)
  (is-a? listener listener%))
  
(define (make-listener [prefix #false])
  (cond [(not prefix) (make-object listener%)]
        [(symbol? prefix) (make-object listener% prefix)]
        [(string? prefix) (make-object listener% (string->symbol prefix))]
        [(number? prefix) (make-object listener% (string->symbol (number->string prefix)))]
        [else (raise-argument-error 'make-listener "(or/c symbol? string? number?)" prefix)]))
  
(module+ deserialize-info
  (provide deserialize-info:listener%))

(define-syntax-rule (define-listener-class name-clause
                      stx-clause ...)
  (define-serializable-class name-clause
    listener%
    stx-clause ...))

(define-syntax-rule (define-deserialize-info deserialize-info-clause ...)
  (module+ deserialize-info
    (provide deserialize-info-clause ...)))
  
(define (http:authentication-url client [permissions '(0)])
  (cond [(not (client? client)) (raise-argument-error 'http:authentication-url "client?" client)]
        [(not (list? permissions) (raise-argument-error 'http:authentication-url "(listof/c integer?)" permissions))]
        [else (format "https://discord.com/api/oauth2/authorize?client_id=~a&permissions=~a&scope=bot%20applications.commands"
                                      (hash-ref (client-user client) 'id)                        
                                      (foldl bitwise-ior 0 permissions))]))
  
(provide (all-defined-out))
#lang racket/base
(require racket/generic
         racket/contract
         json
         "emoji.rkt"
         "role.rkt"
         "sticker.rkt")

(define-generics guild-methods
  (guild-owner? guild-methods id)
  (guild-afk-channel-id guild-methods)
  (guild-afk-timeout guild-methods)
  (guild-application-id guild-methods)
  (guild-banner guild-methods)
  (guild-default-message-notifications guild-methods)
  (guild-description guild-methods)
  (guild-discovery-splash guild-methods)
  (guild-embed-channel-id guild-methods)
  (guild-embed-enabled? guild-methods)
  (guild-emojis guild-methods)
  (guild-explicit-content-filter guild-methods)
  (guild-features guild-methods)
  (guild-home-header guild-methods)
  (guild-hub-type guild-methods)
  (guild-icon guild-methods)
  (guild-id guild-methods)
  (guild-incidents-data guild-methods)
  (guild-latest-onboarding-question-id guild-methods)
  (guild-max-members guild-methods)
  (guild-max-presences guild-methods)
  (guild-max-video-channel-users guild-methods)
  (guild-mfa-level guild-methods)
  (guild-name guild-methods)
  (guild-nsfw? guild-methods)
  (guild-nsfw-level guild-methods)
  (guild-owner-id guild-methods)
  (guild-preferred-locale guild-methods)
  (guild-premium-progress-bar-enabled? guild-methods)
  (guild-premium-subscription-count guild-methods)
  (guild-premium-tier guild-methods)
  (guild-public-updates-channel-id guild-methods)
  (guild-region guild-methods)
  (guild-roles guild-methods)
  (guild-system-channel-flags guild-methods)
  (guild-system-channel-id guild-methods)
  (guild-vanity-url-code guild-methods)
  (guild-verification-level guild-methods)
  (guild-widget-channel-id guild-methods)
  (guild-widget-enabled? guild-methods)
  (guild-rules-channel-id guild-methods)
  (guild-safety-alerts-channel-id guild-methods)
  (guild-splash guild-methods)
  (guild-stickers guild-methods))

(provide
 guild-owner?
 guild-afk-channel-id
 guild-afk-timeout
 guild-application-id 
 guild-banner 
 guild-default-message-notifications 
 guild-description 
 guild-discovery-splash 
 guild-embed-channel-id
 guild-embed-enabled? 
 guild-emojis 
 guild-explicit-content-filter 
 guild-features 
 guild-home-header 
 guild-hub-type
 guild-icon
 guild-id
 guild-incidents-data
 guild-latest-onboarding-question-id
 guild-max-members
 guild-max-presences
 guild-max-video-channel-users
 guild-mfa-level
 guild-name
 guild-nsfw?
 guild-nsfw-level
 guild-owner-id
 guild-preferred-locale
 guild-premium-progress-bar-enabled?
 guild-premium-subscription-count
 guild-premium-tier 
 guild-public-updates-channel-id 
 guild-region 
 guild-roles 
 guild-system-channel-flags
 guild-system-channel-id 
 guild-vanity-url-code
 guild-verification-level 
 guild-widget-channel-id
 guild-widget-enabled? 
 guild-rules-channel-id
 guild-safety-alerts-channel-id
 guild-splash
 guild-stickers)

(define-struct guild
  (data)
  #:methods gen:guild-methods
  [(define (guild-afk-channel-id g)
     (hash-ref (guild-data g) 'afk_channel_id))
   
   (define (guild-afk-timeout g)
     (hash-ref (guild-data g) 'afk_timeout))
   
   (define (guild-application-id g)
     (hash-ref (guild-data g) 'application_id))
   
   (define (guild-banner g)
     (hash-ref (guild-data g) 'banner))
   
   (define (guild-default-message-notifications g)
     (hash-ref (guild-data g) 'default_message_notifications))
   
   (define (guild-description g)
     (hash-ref (guild-data g) 'description))
   
   (define (guild-discovery-splash g)
     (hash-ref (guild-data g) 'discovery_splash))
   
   (define (guild-channel-id g)
     (hash-ref (guild-data g) 'embed_channel_id))
   
   (define (guild-embed-enabled? g)
     (hash-ref (guild-data g) 'embed_enabled))
   
   (define (guild-emojis g)
     (hash-ref (guild-data g) 'emojis))
   
   (define (guild-explicit-content-filter g)
     (hash-ref (guild-data g) 'explicit_content_filter))
   
   (define (guild-features g)
     (hash-ref (guild-data g) 'features))
   
   (define (guild-home-header g)
     (hash-ref (guild-data g) 'home_header))
   
   (define (guild-hub-type g)
     (hash-ref (guild-data g) 'hub_type))

   (define (guild-icon g)
     (hash-ref (guild-data g) 'icon))

   (define (guild-id g)
     (hash-ref (guild-data g) 'id))

   (define (guild-incidents-data g)
     (hash-ref (guild-data g) 'incidents_data))

   (define (guild-latest-onboarding-question-id g)
     (hash-ref (guild-data g) 'latest_onboarding_question_id))

   (define (guild-max-members g)
     (hash-ref (guild-data g) 'max_members))

   (define (guild-max-presences g)
     (hash-ref (guild-data g) 'max_presences))

   (define (guild-max-video-channel-users g)
     (hash-ref (guild-data g) 'max_video_channel_users))

   (define (guild-mfa-level g)
     (hash-ref (guild-data g) 'mfa_level))

   (define (guild-name g)
     (hash-ref (guild-data g) 'name))

   (define (guild-nsfw? g)
     (hash-ref (guild-data g) 'nsfw))

   (define (guild-owner-id g)
     (hash-ref (guild-data g) 'owner_id))

   (define (guild-owner? g id)
     (if (string? id)
         (string=? (guild-owner-id g) id)
         (raise-argument-error 'guild-owner? "string?" id)))

   (define (guild-preferred-locale g)
     (hash-ref (guild-data g) 'preferred_locale))

   (define (guild-premium-progress-bar-enabled? g)
     (hash-ref (guild-data g) 'premium_progress_bar_enabled))

   (define (guild-premium-subscription-count g)
     (hash-ref (guild-data g) 'premium_subscription_count))

   (define (guild-premium-tier g)
     (hash-ref (guild-data g) 'premium_tier))

   (define (guild-public-updates-channel g)
     (hash-ref (guild-data g) 'public_updates_channel_id))

   (define (guild-region g)
     (hash-ref (guild-data g) 'region))

   (define (guild-roles g)
     (hash-ref (guild-data g) 'roles))

   (define (guild-system-channel-flags g)
     (hash-ref (guild-data g) 'system_channel_flags))

   (define (guild-system-channel-id g)
     (hash-ref (guild-data g) 'system_channel_id))

   (define (guild-vanity-url-code g)
     (hash-ref (guild-data g) 'vanity_url_code))

   (define (guild-verification-level g)
     (hash-ref (guild-data g) 'verification_level))

   (define (guild-widget-channel-id g)
     (hash-ref (guild-data g) 'widget_channel_id))

   (define (guild-widget-enabled? g)
     (hash-ref (guild-data g) 'widget_enabled))

   (define (guild-rules-channel-id g)
     (hash-ref (guild-data g) 'rules_channel_id))
   
   (define (guild-safety-alerts-channel-id g)
     (hash-ref (guild-data g) 'safety_alerts_channel_id))

   (define (guild-splash g)
     (hash-ref (guild-data g) 'splash))

   (define (guild-stickers g)
     (hash-ref (guild-data g) 'stickers))])

(provide (contract-out
          [struct guild
            ([data jsexpr?])]))
                  
#lang racket/base

(require (for-syntax racket/base)
         racket-cord
         racket/file
         racket/string)

(define (?? pred obj)
  (if (pred obj) obj #f))

(define star-name "⭐") ;;what emojo should the starboard react to?
(define guild-id "1234") ;;put guild id here
(define starboard-channel-id "1234") ;;put channel id here

;;feed an association list here, but I'm not your dad, do it however you like
(define secrets (make-hash (file->value ".env")))
(define token (hash-ref secrets 'token))

(define starred-messages (make-hash))

(define (not-starred-before? message-id)
  (hash-ref starred-messages message-id #t))

(define (extract-stars data)
  (define reactions (hash-ref data 'reactions))
  (for/sum ([i reactions])
           (cond
             [(string=? (hash-ref (hash-ref i 'emoji) 'name) star-name) (hash-ref i 'count)]
             [else 0])))

(define starboard-bot
  (make-client (symbol->string token) #:intents '(1024) #:token-type 'bot #:auto-shard #t))

(define (on-star-react _ws-client _client data)
  (define emoji (hash-ref (hash-ref data 'emoji) 'name))
  (define message-id (hash-ref data 'message_id))
  (define channel-id (hash-ref data 'channel_id))
  (define message-data (http:get-channel-message _client channel-id message-id))
  (define author-id (hash-ref (hash-ref message-data 'author) 'id))
  (define message-author (http:get-guild-member _client guild-id author-id))
  (define number-of-stars (extract-stars message-data))
  (define mention
    (string-join (list star-name
                       " **"
                       ((λ ()
                          (cond
                            [(?? string? (hash-ref message-author 'nick))]
                            [(?? string? (hash-ref (hash-ref message-author 'user) 'global_name))]
                            [else (hash-ref (hash-ref message-author 'user) 'username)])))
                       "** "
                       star-name)))
  (cond
    [(and (string=? emoji star-name)
          (= number-of-stars 1)
          (not (string=? channel-id starboard-channel-id))
          (not-starred-before? message-id))
     (http:create-message _client starboard-channel-id mention)
     (http:create-message _client
                          starboard-channel-id
                          #:reply-to (make-hash (list (cons 'channel_id channel-id)
                                                      (cons 'guild_id guild-id)
                                                      (cons 'message_id message-id)
                                                      (cons 'type 1))))
     (hash-set! starred-messages message-id #f)]))

(on-event 'raw-message-reaction-add starboard-bot on-star-react)

(with-handlers
    ([exn:break?
      (λ (e)
        (begin
          (update-status starboard-bot #:since (current-milliseconds) #:status "offline" #:afk #t)
          (stop-client starboard-bot)
          (sleep 1)
          (abort-current-continuation (default-continuation-prompt-tag) void)))])
  (begin
    (start-client starboard-bot)
    (update-status starboard-bot #:since (current-milliseconds) #:status "online")))

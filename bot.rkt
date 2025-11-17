#lang racket

(require racket-cord)

(define star-name "⭐")
(define starboard-channel-id "123412341234") ;;put channel id here

;;feed an association list here, but I'm not your dad
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

(define (get-author-nick client guild-id id)
  (define response (http:get-guild-member client guild-id id))
  (hash-ref response 'nick))

(define (get-author-name client guild-id id)
  (define response (http:get-guild-member client guild-id id))
  (hash-ref (hash-ref response 'user) 'global_name))

(define starboard-bot
  (make-client (symbol->string token) #:intents '(36353) #:token-type 'bot #:auto-shard #t))

(define (on-star-react _ws-client _client data)
  (define emoji (hash-ref (hash-ref data 'emoji) 'name))
  (define message-id (hash-ref data 'message_id))
  (define channel-id (hash-ref data 'channel_id))
  (define guild-id (hash-ref data 'guild_id))
  (define message-data (http:get-channel-message _client channel-id message-id))
  (define author-id (hash-ref (hash-ref message-data 'author) 'id))
  (define author-nick (get-author-nick _client guild-id author-id))
  (define author-name (get-author-name _client guild-id author-id))
  (define number-of-stars (extract-stars message-data))
  (define mention
    (string-join (list star-name
                       " **"
                       ((lambda () (if (symbol? author-nick) author-name author-nick)))
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
                                                      (cons 'type 1))))])
  (hash-set! starred-messages message-id #f))

(on-event 'raw-message-reaction-add starboard-bot on-star-react)

(define dr (make-log-receiver discord-logger 'debug))

(thread (thunk (let loop ()
                 (let ([v (sync dr)]) (printf "[~a] ~a\n" (vector-ref v 0) (vector-ref v 1)))
                 (loop))))

(with-handlers
    ([exn:break?
      (lambda (e)
        (begin
          (update-status starboard-bot #:since (current-milliseconds) #:status "offline" #:afk #t)
          (stop-client starboard-bot)
          (sleep 1)
          (display "oof\n")
          (abort-current-continuation (default-continuation-prompt-tag) void)))])
  (begin
    (start-client starboard-bot)
    (update-status starboard-bot #:since (current-milliseconds) #:status "online")))

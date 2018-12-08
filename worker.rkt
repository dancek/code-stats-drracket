#lang racket/base
(require racket/date
         racket/match
         racket/place
         racket/string
         net/url)

(provide start-worker)

(define (submit-pulse base-url token xp)
  (let ((url     (string->url (string-append base-url "/api/my/pulses")))
        (headers (list "User-Agent: code-stats-drracket/0.1.0-alpha"
                       "Content-Type: application/json"
                       (string-append "X-API-Token: " token)))
        ; FIXME: generate json properly
        (data    (string-append "{\"coded_at\":\""
                                (date->string (current-date) #t) "+02:00" ; FIXME
                                "\",\"xps\":[{\"language\":\"Racket\",\"xp\":"
                                (number->string xp)
                                "}]}")))
    (let-values (((status-line headers content-port)
                  (http-sendrecv/url url
                                     #:method #"POST"
                                     #:headers headers
                                     #:data data)))
      (string->number
        (cadr
          (string-split
            (bytes->string/latin-1 status-line)))))))

(define (start-worker pch)
  (match-let (((list host token) (place-channel-get pch)))
    (run-worker pch host token)))

(define (run-worker pch host token)
  (let ((xp (place-channel-get pch)))
    (when (integer? xp)
      (print "xp received")
      (when (<= 200 (submit-pulse host token xp) 299)
        ; success (TODO: what happens on error?
        (place-channel-put pch xp)))
    (when (eq? 'exit xp)
      (exit 0)))
  (run-worker pch host token))

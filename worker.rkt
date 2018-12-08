#lang racket/base
(require racket/date
         racket/format
         racket/match
         racket/place
         racket/string
         net/url)

(provide start-worker)

(define (get-timestamp)
  (let* ((d (current-date))
         (tzoffset (date-time-zone-offset d)))
    (apply format "~a-~a-~aT~a:~a:~a+~a:~a"
           (map (λ (x) (~a x
                           #:align 'right
                           #:min-width 2
                           #:left-pad-string "0"))
                (list (date-year d)
                      (date-month d)
                      (date-day d)
                      (date-hour d)
                      (date-minute d)
                      (date-second d)
                      (quotient tzoffset 3600)
                      (quotient (remainder tzoffset 3600) 60))))))

(define (submit-pulse base-url token xp)
  (let ((url     (string->url (string-append base-url "/api/my/pulses")))
        (headers (list "User-Agent: code-stats-drracket/0.1.0-alpha"
                       "Content-Type: application/json"
                       (string-append "X-API-Token: " token)))
        ; FIXME: generate json properly
        (data    (string-append "{\"coded_at\":\""
                                (get-timestamp)
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
      (let ((status (submit-pulse host token xp)))
        (when (<= 200 status 299)
          ; success (TODO: what happens on error?
          (place-channel-put pch xp))))
    (when (eq? 'exit xp)
      (exit 0)))
  (run-worker pch host token))

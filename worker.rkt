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

(define (submit-pulse base-url token language xp)
  (let ((url     (string->url (string-append base-url "/api/my/pulses")))
        (headers (list "User-Agent: code-stats-drracket/0.1.0-alpha"
                       "Content-Type: application/json"
                       (string-append "X-API-Token: " token)))
        ; FIXME: generate json properly
        (data    (string-append "{\"coded_at\":\""
                                (get-timestamp)
                                "\",\"xps\":[{\"language\":\""
                                language
                                "\",\"xp\":"
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

(define (save-xp host token language xp)
  (let ((status (submit-pulse host token language xp)))
    (when (<= 300 status 499) ; unexpected redirect or client error
      (log-fatal (string-append "Unexpected HTTP status " (number->string status)
                                " from " host ". Stopping code-stats-drracket."))
      (exit 1))
    (when (>= status 500) ; server error; retry
      (log-error (string-append "Server error " (number->string status)
                                " from " host ". Retrying in 10 seconds."))
      (sleep 10)
      (save-xp host token language xp))))

(define (start-worker pch)
  (match-let (((list host token) (place-channel-get pch)))
    (run-worker pch host token)))

; FIXME: can this hang on exit?
(define (run-worker pch host token)
  (match-let (((list language xp) (place-channel-get pch)))
    (save-xp host token language xp))
  (run-worker pch host token))

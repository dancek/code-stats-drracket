#lang racket/base
(require drracket/tool
         framework/preferences
         racket/class
         racket/gui/base
         racket/place
         racket/string
         racket/unit
         mrlib/switchable-button)
(provide tool@)

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

    (define token (getenv "CODESTATS_API_KEY"))
    (define base-url (or (getenv "CODESTATS_API_URL")
                         "https://codestats.net"))

    ; FIXME: collect per-language xp
    (define xp 0)
    (define btn #f)

    (define worker (dynamic-place "worker.rkt" 'start-worker))
    (place-channel-put worker (list base-url token))

    (define (submit-xp language)
      (place-channel-put worker (list language xp))
      (set! xp 0)
      (send btn set-label "C::S 0"))

    (define count-keypresses-mixin
      (mixin ((class->interface text%)) ()
             (define/overment (on-char event)
                              (let ((key-code (send event get-key-code)))
                                (when (or (char? key-code)
                                          (equal? key-code #\backspace)
                                          (equal? key-code #\rubout)) ; delete
                                  (add-xp)))
                              (super on-char event))

             (define/private (add-xp)
               (set! xp (+ xp 1))
               (when btn
                 (send btn set-label
                       (string-append "C::S " (number->string xp)))))

             (super-new)))

    (define xp-counter-mixin
      (mixin (drracket:unit:frame<%>) ()
             (super-new)
             (inherit get-button-panel
                      get-definitions-text
                      register-toolbar-button)

             (define/private (language-name)
               (let* ((defs-text (get-definitions-text))
                      (language-settings (send defs-text get-next-settings))
                      (language (drracket:language-configuration:language-settings-language language-settings)))
                 ; adapted from drracket:rep:extract-language-name
                 (if (is-a? language drracket:module-language:module-language<%>)
                   (car (string-split
                          (send language get-users-language-name defs-text
                                (drracket:language-configuration:language-settings-settings language-settings))
                          ","))
                   (send language get-language-name))))

             (define/augment (on-tab-change from to)
                             (submit-xp (language-name)))

             (set! btn (new switchable-button%
                            (label "Code::Stats")
                            (parent (get-button-panel))
                            (bitmap icon)
                            (callback (λ (_) (submit-xp (language-name))))))

             (register-toolbar-button btn #:number 11)
             (send (get-button-panel) change-children
                   (λ (l)
                      (cons btn (remq btn l))))))

    ; TODO: icon
    (define icon (make-bitmap 16 16))

    (define (phase1) (void))
    (define (phase2) (void))

    (drracket:get/extend:extend-definitions-text count-keypresses-mixin)
    (drracket:get/extend:extend-interactions-text count-keypresses-mixin)
    (drracket:get/extend:extend-unit-frame xp-counter-mixin)))

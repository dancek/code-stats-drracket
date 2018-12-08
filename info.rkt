#lang info
(define drracket-tool-names (list "Code::Stats for DrRacket"))
(define drracket-tools (list (list "tool.rkt")))

(define collection "code-stats-drracket")
(define deps '("base" "drracket-plugin-lib" "gui-lib"))
(define pkg-desc "Code::Stats plugin for DrRacket")
(define version "0.1.0-alpha")
(define pkg-authors '("Hannu Hartikainen <hannu.hartikainen@gmail.com>"))

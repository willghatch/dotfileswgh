#!/usr/bin/env racket
#lang rash
(require racket/cmdline)
(define-values (font character)
  (let ([font "monospace"])
    (command-line
     #:once-each
     [("--font") f "the base font for choosing" (set! font f)]
     #:args (character)
     (values font character))))

;; One liner thanks to https://repolinux.wordpress.com/2013/03/10/find-out-fallback-font-used-by-fontconfig-for-a-certain-character/
env FC_DEBUG=4 pango-view --font=$font -q -t $character #:err stdout-redirect | grep -o "family: \"[^\"]\\+" | cut -c 10- | tail -n 1

#!/usr/bin/env racket
#lang racket/base

(require
 racket/cmdline
 racket/string)

(define input-color-string
  (command-line #:args (input-color-string)
                input-color-string))

(when (string-prefix? input-color-string "#")
  (set! input-color-string (substring input-color-string 1)))

(define red-hex (substring input-color-string 0 2))
(define green-hex (substring input-color-string 2 4))
(define blue-hex (substring input-color-string 4 6))

(define red-int (string->number red-hex 16))
(define green-int (string->number green-hex 16))
(define blue-int (string->number blue-hex 16))

(define foreground-terminal-code (format "38;2;~a;~a;~a" red-int green-int blue-int))
(define background-terminal-code (format "48;2;~a;~a;~a" red-int green-int blue-int))
(printf "start with ^[, end with m, add semicolons to pair with other color codes\n")
(printf "Foreground: ~a\n" foreground-terminal-code)
(printf "Background: ~a\n" background-terminal-code)


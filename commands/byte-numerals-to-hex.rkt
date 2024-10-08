#!/usr/bin/env racket
#lang racket/base

(require
 racket/cmdline
 racket/string
 racket/format
 )

(define (main)
  (define number-strings
    (command-line
     #:usage-help "Converts a series of numbers (< 256) into a hex string"
     #:args numbers
     numbers))
  (when (null? number-strings)
    (error "no numbers provided"))
  (define numbers (map string->number number-strings))

  (printf "~a\n"
          (string-join (map (λ (x) (~r #:base 16
                                       #:min-width 2
                                       #:pad-string "0"
                                       x))
                            numbers)
                       ""
                       #:before-first "0x"))
  )

(main)

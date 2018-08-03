#!/usr/bin/env racket
#lang rash
(require
 racket/cmdline)

(define commands
  (command-line
   ;; default to a different shell than Rash, because when I'm working on
   ;; packages my development rash might be broken, and it would be good to
   ;; see a different prompt anyway to clue me in to being in development.
   #:args ([command "bash"] . rest)
   (cons command rest)))

env CURRENT_DEV_PATH=$RACKET_PKG_DEV_PATH \
    PATH=$RACKET_PKG_DEV_PATH:$PATH \
    CURRENT_DEV_MODE=racket-packages \
    $commands

#!/usr/bin/env racket
#lang rash

;; Motivated by the fact that I have previously always used https urls for
;; working with github, and they are adding friction to using https urls.
;; Thus a conversion script...

(require
 racket/cmdline
 racket/string
 )

(define remote-name (command-line #:args (remote-name) remote-name))

(define orig-url #{git remote get-url $remote-name})
(define new-url (string-replace orig-url "https://github.com/" "git@github.com:"))
echo changing remote $remote-name from $orig-url to $new-url
git remote set-url $remote-name $new-url
